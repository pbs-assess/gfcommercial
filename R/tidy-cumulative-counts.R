# Adapted from gfplot::tidy_sample_avail
tidy_cumulative_counts <- function (data,
                                    years = NULL,
                                    ageing_method_codes = NULL,
                                    variable = c("catch", "samples")) {

  # Summarise silently ---------------------------------------------------------

  options(dplyr.summarise.inform = FALSE)

  # Define area factor levels --------------------------------------------------

  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  # Define species -------------------------------------------------------------

  species_common_name <- unique(data$species_common_name)

  # Define years ---------------------------------------------------------------

  if (is.null(years)) {
    years <- seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1L)
  }

  # Filter by years ------------------------------------------------------------

  data <- data %>%
    dplyr::filter(year %in% years)

  if (nrow(data) > 0) {

    # Calculate week of year -----------------------------------------------------

    if (variable == "catch") {
      data <- data %>%
        dplyr::mutate(week = lubridate::week(fe_start_date))
    } else if (variable == "samples") {
      data <- data %>%
        dplyr::mutate(week = lubridate::week(trip_start_date))
    }

    # Split data for areas and total ---------------------------------------------

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

    # Define counts --------------------------------------------------------------

    if (variable == "catch") {
      counts <- data %>%
        dplyr::group_by(
          species_common_name,
          area,
          year,
          week
        ) %>%
        dplyr::summarise(
          counts = sum(!is.na(landed_kg)))
    } else if (variable == "samples") {
      counts <- data %>%
        dplyr::group_by(
          species_common_name,
          area,
          year,
          week
        ) %>%
        dplyr::summarise(
          counts = sum(!is.na(unique(specimen_id)))
        )
    }

    # Remove data points where week is NA

    counts <- subset(counts,
                     !is.na(week))

    # Fill in missing weeks with zero ------------------------------------------

    labels <- data.frame(species_common_name = unique(counts$species_common_name),
                         area = rep(area_levels,
                                    each = 53*length(years)),
                         year = rep(years,
                                    each = 53,
                                    times = length(area_levels)),
                         week = rep(1:53,
                                    times = length(area_levels)*length(years))
                         )

    counts <- dplyr::right_join(counts,
                                labels,
                                by = c("species_common_name", "area", "year", "week"),
                                keep = FALSE)

    counts[is.na(counts)] <- 0

    counts <- counts %>%
      dplyr::arrange(area, year, week)

    counts$area <- factor(counts$area, levels = area_levels)

    counts <- counts %>%
      dplyr::arrange(area)

  } else {

    # Create an empty dataframe

    counts <- data.frame(species_common_name = species_common_name,
                         area = rep(area_levels,
                                    each = 53*length(years)),
                         year = rep(years,
                                    each = 53,
                                    times = length(area_levels)),
                         week = rep(1:53,
                                    times = length(area_levels)*length(years)),
                         counts = 0
                         )

    counts[is.na(counts)] <- 0

  }

  # Cumulative sum of counts ---------------------------------------------------

  cumulative <- counts %>%
    dplyr::group_by(
      species_common_name,
      area,
      year
    ) %>%
    dplyr::mutate(
      cumulative_counts = cumsum(counts)
    )

  # Calculate the proportion of the cumulative sum of each variable
  proportions <- cumulative %>%
    dplyr::group_by(
      species_common_name,
      area,
      year
    ) %>%
    dplyr::mutate(proportion = cumulative_counts/max(cumulative_counts))

  proportions[is.na(proportions)] <- 0

  proportions <- proportions %>%
    dplyr::group_by(
      species_common_name,
      area,
      year
    ) %>%
    dplyr::mutate(n = max(cumulative_counts))

  # Return counts --------------------------------------------------------------

  return(proportions)
}
