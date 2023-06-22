tidy_ages_by_areas_raw <- function (data,
                                    year_range = NULL,
                                    ...) {

  # Define area factor levels --------------------------------------------------

  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  # Define years ---------------------------------------------------------------

  years <- seq(min(year_range), max(year_range))

  # Areas ----------------------------------------------------------------------

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
    area_5E <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "5E"
                          )
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
    area_5D <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "5D"
    )
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
    area_5C <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "5C"
    )
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
    area_5B <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "5B"
    )
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
    area_5A <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "5A"
    )
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
    area_3D <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "3D"
    )
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
    area_3C <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "3C"
    )
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
    area_4B <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "4B"
    )
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
    area_total <- data.frame(species_common_name = unique(data$species_common_name),
                          survey_abbrev = "Commercial",
                          year = rep(years, each = 2),
                          sex = c("M", "F"),
                          age = 0,
                          proportion = 0,
                          total = 0,
                          area = "Total"
    )
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




### Tidy lengths by area

tidy_lengths_by_areas_raw <- function (data, year_range = NULL, total, ...) {

  # Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  # Define years
  years <- seq(min(year_range), max(year_range))

  if (total == TRUE) {

    # Merge sexes
    data <- data %>% dplyr::mutate(data, sex = 2)

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
      area_5E <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5E"
      )
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
      area_5D <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5D"
      )
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
      area_5C <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5C"
      )
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
      area_5B <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5B"
      )
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
      area_5A <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5A"
      )
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
      area_3D <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "3D"
      )
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
      area_3C <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "3C"
      )
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
      area_4B <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "4B"
      )
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
      area_total <- data.frame(species_common_name = unique(data$species_common_name),
                               survey_abbrev = "Commercial",
                               year = rep(years, each = length(seq(20, 80, by = 10))),
                               length_bin = rep(seq(20, 80, by = 10), times = length(years)),
                               proportion = NA,
                               total = NA,
                               area = "Total"
      )
    }
  }

  if (total == FALSE) {

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
        dplyr::mutate(area = factor("5E", levels = area_levels))
    } else {
      area_5E <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5E"
      )
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
        dplyr::mutate(area = factor("5D", levels = area_levels))
    } else {
      area_5D <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5D"
      )
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
        dplyr::mutate(area = factor("5C", levels = area_levels))
    } else {
      area_5C <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5C"
      )
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
        dplyr::mutate(area = factor("5B", levels = area_levels))
    } else {
      area_5B <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5B"
      )
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
        dplyr::mutate(area = factor("5A", levels = area_levels))
    } else {
      area_5A <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "5A"
      )
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
        dplyr::mutate(area = factor("3D", levels = area_levels))
    } else {
      area_3D <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "3D"
      )
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
        dplyr::mutate(area = factor("3C", levels = area_levels))
    } else {
      area_3C <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "3C"
      )
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
        dplyr::mutate(area = factor("4B", levels = area_levels))
    } else {
      area_4B <- data.frame(species_common_name = unique(data$species_common_name),
                            survey_abbrev = "Commercial",
                            year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                            sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                            length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                            proportion = NA,
                            total = NA,
                            area = "4B"
      )
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
        dplyr::mutate(area = factor("Total", levels = area_levels))
    } else {
      area_total <- data.frame(species_common_name = unique(data$species_common_name),
                               survey_abbrev = "Commercial",
                               year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                               sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                               length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                               proportion = NA,
                               total = NA,
                               area = "Total"
      )
    }

    # Return
    lengths <- dplyr::bind_rows(area_5E,
                                area_5D,
                                area_5C,
                                area_5B,
                                area_5A,
                                area_3D,
                                area_3C,
                                area_4B,
                                area_total)

    blanks <- data.frame(species_common_name = unique(data$species_common_name),
                         survey_abbrev = "Commercial",
                         year = rep(years, each = length(area_levels) * 2),
                         sex = rep(c("M", "F") , times = length(years) * length(area_levels)),
                         length_bin = rep(mean(lengths_sex$length_bin), each = 2, times = length(years)*length(area_levels)),
                         proportion = NA,
                         total = NA,
                         area = area_levels
    )


  }


  # Return
lengths <- dplyr::bind_rows(area_5E,
                            area_5D,
                            area_5C,
                            area_5B,
                            area_5A,
                            area_3D,
                            area_3C,
                            area_4B,
                            area_total
                            )

blanks <- data.frame(species_common_name = unique(data$species_common_name),
                         survey_abbrev = "Commercial",
                         year = rep(years, each = length(seq(20, 80, by = 10)) * 2),
                         sex = rep(c("M", "F") , times = length(years) * length(seq(20, 80, by = 10))),
                         length_bin = rep(seq(20, 80, by = 10), each = 2, times = length(years)),
                         proportion = NA,
                         total = NA,
                         area = "Total"
)


}
