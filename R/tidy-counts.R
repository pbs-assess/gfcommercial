# Adapted from gfplot::tidy_sample_avail
tidy_commercial_counts <- function (data,
                                    years = NULL,
                                    ageing_method_codes = NULL) {
  
  # Summarise silently ---------------------------------------------------------
  
  options(dplyr.summarise.inform = FALSE)
  
  # Define area factor levels --------------------------------------------------
  
  area_levels <- c("5E", "5CD", "5AB", "3CD", "4B", "Total")
  
  # Define years ---------------------------------------------------------------
  
  if (is.null(years)) {
    years <- seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1L)
  }
  
  # Filter by years ------------------------------------------------------------
  
  data <- data %>%
    dplyr::filter(.data$year %in% years)
  
  # Remove duplicate specimens -------------------------------------------------
  
  data <- data %>%
    dplyr::distinct(.data$specimen_id, .keep_all = TRUE)
  
  # Conditionally filter by ageing method code ---------------------------------
  
  if (!is.null(ageing_method_codes)) {
    data <- data %>%
      dplyr::filter(.data$ageing_method %in% ageing_method_codes)
  }
  
  # Conditionally augment ------------------------------------------------------
  
  if (!"age_specimen_collected" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(age_specimen_collected = NA)
  }
  
  # Split data for areas and total ---------------------------------------------
  
  # Areas
  data_areas <- data %>%
    dplyr::mutate(
      area_chars = substr(.data$major_stat_area_name, 1, 2),
      area = ifelse(
        .data$area_chars == "5E", "5E",
        ifelse(
          .data$area_chars %in% c("5C", "5D"), "5CD",
          ifelse(
            .data$area_chars %in% c("5A", "5B"), "5AB",
            ifelse(
              .data$area_chars %in% c("3C", "3D"), "3CD",
              ifelse(
                .data$area_chars == "4B", "4B", NA_character_
              )
            )
          )
        )
      )
    ) %>%
    dplyr::select(-.data$area_chars) %>%
    tidyr::drop_na(.data$area)
  # Total
  data_total <- data_areas %>%
    dplyr::mutate(area = "Total")
  # Bind rows
  data <- dplyr::bind_rows(data_areas, data_total) %>%
    dplyr::mutate(area = factor(.data$area, levels = area_levels))
  
  # Define counts --------------------------------------------------------------
  
  counts <- data %>%
    dplyr::group_by(
      .data$species_common_name,
      .data$area,
      .data$year
    ) %>%
    dplyr::summarise(
      age = sum(!is.na(.data$age) & .data$age > 0),
      ageing_structure = sum(
        !is.na(.data$age_specimen_collected) &
          .data$age_specimen_collected == 1
      ),
      length = sum(!is.na(.data$length) & .data$length > 0),
      weight = sum(!is.na(.data$weight) & .data$weight > 0),
      maturity = sum(!is.na(.data$maturity_code) & .data$maturity_code > 0)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = .data$age:.data$maturity,
      names_to = "type",
      values_to = "n"
    ) %>%
    dplyr::mutate(
      type = factor(
        .data$type,
        levels = c("age", "ageing_structure", "length", "weight", "maturity")
      )
    ) %>%
    dplyr::arrange(
      .data$species_common_name,
      .data$area,
      .data$type,
      .data$year
    ) %>%
    tidyr::replace_na(replace = list(n = 0))
  
  # Return counts --------------------------------------------------------------
  
  return(counts)
}