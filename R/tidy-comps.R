tidy_ages_by_areas_raw <- function (data, ...) {
  
  # Define area factor levels
  area_levels <- c("5E", "5CD", "5AB", "3CD", "4B", "Total")
  # Area 5E
  suppressWarnings(
    area_5E <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5E",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5E)) {
    area_5E <- area_5E %>%
      dplyr::mutate(area = factor("5E", levels = area_levels))
  } else {
    area_5E <- NULL
  }
  # Area 5CD
  suppressWarnings(
    area_5CD <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5[CD]+",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5CD)) {
    area_5CD <- area_5CD %>%
      dplyr::mutate(area = factor("5CD", levels = area_levels))
  } else {
    area_5CD <- NULL
  }
  # Area 5AB
  suppressWarnings(
    area_5AB <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5[AB]+",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5AB)) {
    area_5AB <- area_5AB %>%
      dplyr::mutate(area = factor("5AB", levels = area_levels))
  } else {
    area_5AB <- NULL
  }
  # Area 3CD
  suppressWarnings(
    area_3CD <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "3[CD]+",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_3CD)) {
    area_3CD <- area_3CD %>%
      dplyr::mutate(area = factor("3CD", levels = area_levels))
  } else {
    area_3CD <- NULL
  }
  # Area 4B
  suppressWarnings(
    area_4B <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "4B",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_4B)) {
    area_4B <- area_4B %>%
      dplyr::mutate(area = factor("4B", levels = area_levels))
  } else {
    area_4B <- NULL
  }
  # Area total
  suppressWarnings(
    area_total <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "*",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_total)) {
    area_total <- area_total %>%
      dplyr::mutate(area = factor("Total", levels = area_levels))
  } else {
    area_total <- NULL
  }
  # Return
  dplyr::bind_rows(
    area_5E,
    area_5CD,
    area_5AB,
    area_3CD,
    area_4B,
    area_total
  )
}

tidy_lengths_by_areas_raw <- function (data, ...) {
  
  # Merge sexes
  data <- data %>% dplyr::mutate(data, sex = 2)
  # Define area factor levels
  area_levels <- c("5E", "5CD", "5AB", "3CD", "4B", "Total")
  # Area 5E
  suppressWarnings(
    area_5E <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5E",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5E)) {
    area_5E <- area_5E %>%
      dplyr::mutate(area = factor("5E", levels = area_levels)) %>%
      dplyr::select(-.data$sex)
  } else {
    area_5E <- NULL
  }
  # Area 5CD
  suppressWarnings(
    area_5CD <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5[CD]+",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5CD)) {
    area_5CD <- area_5CD %>%
      dplyr::mutate(area = factor("5CD", levels = area_levels)) %>%
      dplyr::select(-.data$sex)
  } else {
    area_5CD <- NULL
  }
  # Area 5AB
  suppressWarnings(
    area_5AB <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5[AB]+",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5AB)) {
    area_5AB <- area_5AB %>%
      dplyr::mutate(area = factor("5AB", levels = area_levels)) %>%
      dplyr::select(-.data$sex)
  } else {
    area_5AB <- NULL
  }
  # Area 3CD
  suppressWarnings(
    area_3CD <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "3[CD]+",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_3CD)) {
    area_3CD <- area_3CD %>%
      dplyr::mutate(area = factor("3CD", levels = area_levels)) %>%
      dplyr::select(-.data$sex)
  } else {
    area_3CD <- NULL
  }
  # Area 4B
  suppressWarnings(
    area_4B <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "4B",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_4B)) {
    area_4B <- area_4B %>%
      dplyr::mutate(area = factor("4B", levels = area_levels)) %>%
      dplyr::select(-.data$sex)
  } else {
    area_4B <- NULL
  }
  # Area total
  suppressWarnings(
    area_total <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "*",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_total)) {
    area_total <- area_total %>%
      dplyr::mutate(area = factor("Total", levels = area_levels)) %>%
      dplyr::select(-.data$sex)
  } else {
    area_total <- NULL
  }
  # Return
  dplyr::bind_rows(
    area_5E,
    area_5CD,
    area_5AB,
    area_3CD,
    area_4B,
    area_total
  )
}
