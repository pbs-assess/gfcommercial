# Adapted from gfplot::tidy_sample_avail
tidy_cumulative_counts <- function (data,
                                    FE_dat = NULL,
                                    years = NULL,
                                    ageing_method_codes = NULL,
                                    variable = c("catch", "samples", "spatial")) {

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

    # Calculate week of year ---------------------------------------------------

    if (variable == "catch") {
      data <- data %>%
        dplyr::mutate(week = lubridate::week(best_date))
    } else {
      data <- data %>%
        dplyr::mutate(week = lubridate::week(trip_start_date))
    }

    # Split data for areas and total -------------------------------------------

    # Areas
    areas <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B")
    data_areas <- data %>%
      dplyr:: mutate(area = assign_areas(major_stat_area_name, areas))
    data_areas <- data_areas[!is.na(data_areas$area), , drop = FALSE]

    # Total
    data_total <- data_areas %>%
      dplyr::mutate(area = "Total")
    # Bind rows
    data <- dplyr::bind_rows(data_areas, data_total) %>%
      dplyr::mutate(area = factor(area, levels = area_levels))

    # Define counts --------------------------------------------------------------

    if (variable == "catch") {
      temp <- data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(total_kg = sum(c(landed_kg, discarded_kg))) %>%
        dplyr::ungroup()


      counts <- temp %>%
        dplyr::group_by(
          species_common_name,
          area,
          year,
          week
        ) %>%
        dplyr::summarise(
          counts = sum(total_kg)) %>%
        dplyr::ungroup()

    } else if (variable == "samples") {
      # Filter out recreational samples
      data <- data |>
        dplyr::filter(trip_sub_type_desc != "RECREATIONAL",
                      gear_desc != "RECREATIONAL ROD & REEL")

      counts <- data %>%
        dplyr::group_by(
          species_common_name,
          area,
          year,
          week
        ) %>%
        dplyr::summarise(
          counts = n()
        )%>%
        ungroup()

    } else if (variable == "spatial") {
      # Filter out recreational samples
      data <- data |>
        dplyr::filter(trip_sub_type_desc != "RECREATIONAL",
                      gear_desc != "RECREATIONAL ROD & REEL")

      # Join with fishing event data to get spatially explicit data

      if (!is.null(FE_dat)) {
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

        # All spatially explicit specimens must have degrees and minutes for latitude or longitude, start or finish

        data <- dplyr::left_join(data, FE_dat, by = join_by(trip_id == TRIP_ID, fishing_event_id == FISHING_EVENT_ID))

        # Check that all spatial columns within each row are non-NA using dplyr
        data <- data |>
          dplyr::rowwise() |> # So that sum() in the next line sums within each row
          dplyr::mutate(start_latlong = sum(FE_START_LATTITUDE_DEGREE, FE_START_LATTITUDE_MINUTE, FE_START_LONGITUDE_DEGREE, FE_START_LONGITUDE_MINUTE)) |>
          dplyr::mutate(end_latlong = sum(FE_END_LATTITUDE_DEGREE, FE_END_LATTITUDE_MINUTE, FE_END_LONGITUDE_DEGREE, FE_END_LONGITUDE_MINUTE)) |>
          dplyr::mutate(spatial = as.integer(sum(start_latlong, end_latlong, na.rm = TRUE) >= 1))
      }

      counts <- data %>%
        dplyr::group_by(
          species_common_name,
          area,
          year,
          week
        ) %>%
        dplyr::summarise(
          counts_samples = n(),
          counts = sum(spatial == 1)
        ) %>%
        ungroup()
    }

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

  if (variable == "spatial") {
    cumulative <- counts %>%
      dplyr::group_by(
        species_common_name,
        area,
        year
      ) %>%
      dplyr::mutate(
        cumulative_samples = cumsum(counts_samples),
        cumulative_spatial = cumsum(counts)
      )

    # Calculate the proportion of the cumulative sum of each variable
    proportions <- cumulative %>%
      dplyr::group_by(
        species_common_name,
        area,
        year
      ) %>%
      #dplyr::mutate(proportion = cumulative_spatial/max(cumulative_samples),
      dplyr::mutate(proportion = cumulative_spatial/max(cumulative_spatial),
                    n = max(cumulative_spatial, na.rm = TRUE)
                    )

    proportions[is.na(proportions)] <- 0

    proportions <- proportions |>
      dplyr::select(-counts_samples, -cumulative_samples)

  } else {
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
      dplyr::mutate(proportion = cumulative_counts/max(cumulative_counts),
                    n = max(cumulative_counts, na.rm = TRUE)
                    )

    proportions[is.na(proportions)] <- 0

  }


  # Return counts --------------------------------------------------------------

  return(proportions)
}
