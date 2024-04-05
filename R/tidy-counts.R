# Adapted from gfplot::tidy_sample_avail
# a function to tidy commercial count data
tidy_commercial_counts <- function (data,
                                    years = NULL,
                                    ageing_method_codes = NULL) {

  # Summarise silently ---------------------------------------------------------

  options(dplyr.summarise.inform = FALSE)

  # Define species -------------------------------------------------------------

  spp <- unique(data$species_common_name)

  # Define area factor levels --------------------------------------------------

  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  # Define count type ----------------------------------------------------------

  type <- c("age", "ageing_structure", "length", "weight", "maturity", "spatial", "fishing_events")

  # Define years ---------------------------------------------------------------

  if (is.null(years)) {
    years <- seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1L)
  }

  # Filter by years ------------------------------------------------------------

  data <- data %>%
    dplyr::filter(year %in% years)

  if (nrow(data) > 0) {

    # Remove duplicate specimens -----------------------------------------------

    data <- data %>%
      dplyr::distinct(specimen_id, .keep_all = TRUE)

    # Conditionally filter by ageing method code -------------------------------

    if (!is.null(ageing_method_codes)) {
      data <- data %>%
        dplyr::filter(ageing_method %in% ageing_method_codes)
    }

    # Conditionally augment ----------------------------------------------------

    if (!"age_specimen_collected" %in% colnames(data)) {
      data <- data %>%
        dplyr::mutate(age_specimen_collected = NA)
    }

    # Split data for areas and total -------------------------------------------

    # Areas
    data_areas <- data %>%
      dplyr::mutate(
        area_chars = substr(major_stat_area_name, 1, 2),
        area = ifelse(
          area_chars == "5E", "5E",
          ifelse(
            area_chars %in% "5D", "5D",
            ifelse(
              area_chars %in% "5C", "5C",
              ifelse(
                area_chars %in% "5B", "5B",
                ifelse(
                  area_chars %in% "5A", "5A",
                  ifelse(
                    area_chars %in% "3D", "3D",
                    ifelse(
                      area_chars %in% "3C", "3C",
                      ifelse(
                        area_chars == "4B", "4B", NA_character_
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) %>%
      dplyr::select(-area_chars) %>%
      tidyr::drop_na(area)
    # Total
    data_total <- data_areas %>%
      dplyr::mutate(area = "Total")
    # Bind rows
    data <- dplyr::bind_rows(data_areas, data_total) %>%
      dplyr::mutate(area = factor(area, levels = area_levels))

    # Filter out recreational sourced data
    data <- data %>%
      dplyr::filter(trip_sub_type_desc != "RECREATIONAL")

    # Join with fishing event data to get spatially explicit data

    FE_dat <- read.csv(paste0(here::here("data"), "/", "fishing_event.csv"))

    FE_dat <- FE_dat %>%
      dplyr::select(TRIP_ID, FISHING_EVENT_ID, MAJOR_STAT_AREA_CODE, MINOR_STAT_AREA_CODE, DFO_STAT_AREA_CODE, DFO_STAT_SUBAREA_CODE, # Select relevant columns
                    FE_START_LATTITUDE_DEGREE, FE_START_LATTITUDE_MINUTE, FE_START_LONGITUDE_DEGREE, FE_START_LONGITUDE_MINUTE,
                    FE_END_LATTITUDE_DEGREE, FE_END_LATTITUDE_MINUTE, FE_END_LONGITUDE_DEGREE, FE_END_LONGITUDE_MINUTE) %>%
      dplyr::mutate(FE_START_LATTITUDE_MINUTE = as.numeric(FE_START_LATTITUDE_MINUTE), # Turn character variables into numeric
                    FE_START_LONGITUDE_DEGREE = as.numeric(FE_START_LONGITUDE_DEGREE),
                    FE_START_LONGITUDE_MINUTE = as.numeric(FE_START_LONGITUDE_MINUTE),
                    FE_END_LATTITUDE_DEGREE = as.numeric(FE_END_LATTITUDE_DEGREE),
                    FE_END_LATTITUDE_MINUTE = as.numeric(FE_END_LATTITUDE_MINUTE),
                    FE_END_LONGITUDE_DEGREE = as.numeric(FE_END_LONGITUDE_DEGREE),
                    FE_END_LONGITUDE_MINUTE = as.numeric(FE_END_LONGITUDE_MINUTE)) %>%
      unique()

    # All spatially explicit specimens must have degreens and minutes for latitude or longitude, start or finish
    # Confirmed April 5, 2024


    data <- dplyr::left_join(data, FE_dat, by = join_by(trip_id == TRIP_ID, fishing_event_id == FISHING_EVENT_ID), keep = TRUE)


    # Define counts ------------------------------------------------------------

    counts <- data %>%
      dplyr::group_by(
        species_common_name,
        area,
        year
      ) %>%
      dplyr::summarise(
        age = sum(!is.na(age) & age > 0),
        ageing_structure = sum(
          !is.na(age_specimen_collected) &
            age_specimen_collected == 1),
        length = sum(!is.na(length) & length > 0),
        weight = sum(!is.na(weight) & weight > 0),
        maturity = sum(!is.na(maturity_code) & maturity_code > 0),
        spatial = sum(!is.na(FE_START_LATTITUDE_DEGREE)),
        fishing_events = sum(!is.na(unique(fishing_event_id)))
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols = age:fishing_events,
        names_to = "type",
        values_to = "n"
      ) %>%
      dplyr::mutate(
        type = factor(
          type,
          levels = c("age", "ageing_structure", "length", "weight", "maturity", "spatial", "fishing_events")
        )
      ) %>%
      dplyr::arrange(
        species_common_name,
        area,
        type,
        year
      ) %>%
      tidyr::replace_na(replace = list(n = 0))

  } else {

    counts <- data.frame(species_common_name = spp,
                         area = rep(area_levels, each = length(years)*length(type)),
                         type = rep(type, each = length(years), times = length(area_levels)),
                         year = rep(years, times = length(area_levels)*length(type)),
                         n = 0
                         )
    counts <- counts %>%
      dplyr::mutate(area = factor(area, levels = area_levels))

  }


  # Return counts --------------------------------------------------------------

  return(counts)
}
