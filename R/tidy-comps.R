tidy_ages_by_areas_raw <- function (data, ...) {

  # Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")
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
  # Area 5D
  suppressWarnings(
    area_5D <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5D",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5D)) {
    area_5D <- area_5D %>%
      dplyr::mutate(area = factor("5D", levels = area_levels))
  } else {
    area_5D <- NULL
  }
  # Area 5C
  suppressWarnings(
    area_5C <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5C",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5C)) {
    area_5C <- area_5C %>%
      dplyr::mutate(area = factor("5C", levels = area_levels))
  } else {
    area_5C <- NULL
  }
  # Area 5B
  suppressWarnings(
    area_5B <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5B",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5B)) {
    area_5B <- area_5B %>%
      dplyr::mutate(area = factor("5B", levels = area_levels))
  } else {
    area_5B <- NULL
  }
  # Area 5A
  suppressWarnings(
    area_5A <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5A",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5A)) {
    area_5A <- area_5A %>%
      dplyr::mutate(area = factor("5A", levels = area_levels))
  } else {
    area_5A <- NULL
  }
  # Area 3D
  suppressWarnings(
    area_3D <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "3D",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_3D)) {
    area_3D <- area_3D %>%
      dplyr::mutate(area = factor("3D", levels = area_levels))
  } else {
    area_3D <- NULL
  }
  # Area 3C
  suppressWarnings(
    area_3C <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "3C",
      age_length = "age",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_3C)) {
    area_3C <- area_3C %>%
      dplyr::mutate(area = factor("3C", levels = area_levels))
  } else {
    area_3C <- NULL
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
    area_5D,
    area_5C,
    area_5B,
    area_5A,
    area_3D,
    area_3C,
    area_4B,
    area_total
  )
}

tidy_lengths_by_areas_raw <- function (data, ...) {

  # Merge sexes
  data <- data %>% dplyr::mutate(data, sex = 2)
  # Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")
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
      dplyr::select(-sex)
  } else {
    area_5E <- NULL
  }
  # Area 5D
  suppressWarnings(
    area_5D <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5D",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5D)) {
    area_5D <- area_5D %>%
      dplyr::mutate(area = factor("5D", levels = area_levels)) %>%
      dplyr::select(-sex)
  } else {
    area_5D <- NULL
  }
  # Area 5C
  suppressWarnings(
    area_5C <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5C",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5C)) {
    area_5C <- area_5C %>%
      dplyr::mutate(area = factor("5C", levels = area_levels)) %>%
      dplyr::select(-sex)
  } else {
    area_5C <- NULL
  }
  # Area 5B
  suppressWarnings(
    area_5B <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5B",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5B)) {
    area_5B <- area_5B %>%
      dplyr::mutate(area = factor("5B", levels = area_levels)) %>%
      dplyr::select(-sex)
  } else {
    area_5B <- NULL
  }
  # Area 5A
  suppressWarnings(
    area_5A <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "5A",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_5A)) {
    area_5A <- area_5A %>%
      dplyr::mutate(area = factor("5A", levels = area_levels)) %>%
      dplyr::select(-sex)
  } else {
    area_5A <- NULL
  }
  # Area 3D
  suppressWarnings(
    area_3D <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "3D",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_3D)) {
    area_3D <- area_3D %>%
      dplyr::mutate(area = factor("3D", levels = area_levels)) %>%
      dplyr::select(-sex)
  } else {
    area_3D <- NULL
  }
  # Area 3C
  suppressWarnings(
    area_3C <- gfplot::tidy_comps(
      dat = data,
      ...,
      area_grep_pattern = "3C",
      age_length = "length",
      frequency_type = "raw"
    )
  )
  if (tibble::is_tibble(area_3C)) {
    area_3C <- area_3C %>%
      dplyr::mutate(area = factor("3C", levels = area_levels)) %>%
      dplyr::select(-sex)
  } else {
    area_3C <- NULL
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
      dplyr::select(-sex)
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
      dplyr::select(-sex)
  } else {
    area_total <- NULL
  }
  # Return
  dplyr::bind_rows(
    area_5E,
    area_5D,
    area_5C,
    area_5B,
    area_5A,
    area_3D,
    area_3C,
    area_4B,
    area_total
  )
}
