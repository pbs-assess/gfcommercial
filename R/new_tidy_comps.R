# Adopted from gfplot
bin_lengths <- function(dat, value, bin_size) {
  value <- enquo(value)
  bin_range <- dat %>%
    select(!!value) %>%
    pull() %>%
    range()
  bin_range[1] <- round_down_even(bin_range[1])
  bin_range[2] <- ceiling(bin_range[2])
  bins <- seq(min(bin_range), max(bin_range), by = bin_size)
  mutate(dat, !!quo_name(value) :=
           bins[findInterval(!!value, bins)] + bin_size / 2)
}

# Adopted from gfplot
round_down_even <- function(x, base = 2) {
  base * floor(x / base)
}

# Adopted from gfplot
tidy_lengths_comm_raw <- function(dat,
                                  years = NULL,
                                  areas = NULL,
                                  area_grep_pattern = "*",
                                  ageing_method_codes = NULL,
                                  usability_codes = c(0, 1, 2, 6),
                                  sorted,
                                  bin_size = 2,
                                  age_length = "length",
                                  sample_type = "commercial",
                                  frequency_type = "raw",
                                  remove_unsexed = FALSE,
                                  ...) {

  # -------------------------------------------
  # Filter down data (basics):
  spp <- unique(dat$species_common_name)

  avg_length <- round_down_even(mean(dat$length))

  if (remove_unsexed) {
    dat <- dat %>%
      dplyr::filter(sex %in% c(1, 2)
    )
    dat$sex <- dplyr::case_when(
      dat$sex == 1 ~ "M",
      dat$sex == 2 ~ "F"
    )
  } else {
    # Merge sexes
    dat <- dat %>%
      dplyr::mutate(sex = 2)
  }

  dat <- dat %>%
    dplyr::filter(!is.na(year))

  if (is.null(years)) years <- seq(min(dat$year), max(dat$year), by = 1)

  dat <- dat %>%
    dplyr::filter(year >= min(years), year <= max(years))

  # -------------------------------------------
  # Filter down usability codes:
  if (!is.null(usability_codes)) {
    dat <- dat %>%
      dplyr::filter(usability_code %in% usability_codes)
  }

  # Filter down sampling description (sorted vs unsorted, unknown excluded):
  if (sorted) {
    dat <- dat %>%
      dplyr::filter(sampling_desc == "KEEPERS" | sampling_desc == "DISCARDS")
  } else if (!sorted) {
    dat <- dat %>%
      dplyr::filter(sampling_desc == "UNSORTED")
  }

  # -------------------------------------------
  # Filter down data (commercial):
  if (sample_type == "commercial") {
    if (nrow(dat) == 0) {
      if (!is.null(areas)) {
        if (remove_unsexed) {
          blank_areas <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep(areas, each = length(years)*2),
                                    year = rep(years, each = 2, times = length(areas)),
                                    sex = rep(c("M", "F"), times = length(years)*length(areas)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        } else {
          blank_areas <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep(areas, each = length(years)),
                                    year = rep(years, times = length(areas)),
                                    sex = rep(2, times = length(years)*length(areas)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        }

        return(blank_areas)

        # If no data, create a blank data frame
      } else {
        if (remove_unsexed) {
          blank_total <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep("Total", each = length(years)*2),
                                    year = rep(years, each = 2),
                                    sex = rep(c("M", "F"), times = length(years)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        } else {
          blank_total <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep("Total", times = length(years)),
                                    year = years,
                                    sex = rep(2, times = length(years)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        }

        return(blank_total)

      }
    }

    dat$survey_abbrev <- "Commercial"

    pbs_areas <- gfplot::pbs_areas[grep(
      area_grep_pattern,
      gfplot::pbs_areas$major_stat_area_description
    ), , drop = FALSE]

    dat <- dplyr::semi_join(dat, pbs_areas, by = "major_stat_area_code")
  }

  # Assign areas (or total)
  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat <- dat %>%
      dplyr::mutate(area = "Total")
  }

  # -------------------------------------------
  # Filter down data (lengths):
  if (age_length == "length") {
    dat <- dat %>%
      dplyr::filter(!is.na(length))

    if (nrow(dat) == 0) {
      if (!is.null(areas)) {
        if (remove_unsexed) {
          blank_areas <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep(areas, each = length(years)*2),
                                    year = rep(years, each = 2, times = length(areas)),
                                    sex = rep(c("M", "F"), times = length(years)*length(areas)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        } else {
          blank_areas <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep(areas, each = length(years)),
                                    year = rep(years, times = length(areas)),
                                    sex = rep(2, times = length(years)*length(areas)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        }

        return(blank_areas)

        # If no data, create a blank data frame
      } else {
        if (remove_unsexed) {
          blank_total <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep("Total", each = length(years)*2),
                                    year = rep(years, each = 2),
                                    sex = rep(c("M", "F"), times = length(years)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        } else {
          blank_total <- data.frame(species_common_name = spp,
                                    survey_abbrev = "Commercial",
                                    area = rep("Total", times = length(years)),
                                    year = years,
                                    sex = rep(2, times = length(years)),
                                    length_bin = avg_length,
                                    proportion = NA,
                                    total = NA
          )
        }

        return(blank_total)

      }
    }
  }

  # Remove duplicate specimens
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical

  # -------------------------------------------
  # Retain only necessary columns:
  if (frequency_type == "raw") {
    dat <- dat %>%
      dplyr::select(species_common_name, survey_abbrev, year, sex, area, length)
  }

  # -------------------------------------------
  # Calculate the actual age or length frequencies:

  # -------------------------------------------
  # Raw (commercial or survey):
  if (age_length == "length" && frequency_type == "raw") {
    freq <- dat %>%
      dplyr::do(bin_lengths(dat, value = length, bin_size = bin_size)) %>%
      dplyr::rename(length_bin = length) %>%
      dplyr::group_by(species_common_name, year, length_bin, sex, survey_abbrev, area) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::group_by(year, survey_abbrev) %>%
      dplyr::mutate(proportion = n / sum(n)) %>%
      dplyr::select(-n) %>%
      dplyr::ungroup()
  }

  # -------------------------------------------
  # Join in the counts for labels:
  if (age_length == "length") { # also by year
    counts <- dat %>%
      dplyr::group_by(year, species_common_name, survey_abbrev, area) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::ungroup()

    freq <- dplyr::left_join(freq, counts,
                             by = c("species_common_name", "year", "survey_abbrev", "area"))

    freq <- freq %>%
      dplyr::select(species_common_name, survey_abbrev, year, area, sex,
                    length_bin,proportion, total)
  }

  # ------------------------------------------
  # Join frequency and count data frame with labels to fill in blanks
  if (remove_unsexed == FALSE) {
    # Sexed and unsexed by area
    if (!is.null(areas)) {

      labels <- data.frame(species_common_name = unique(dat$species_common_name),
                           survey_abbrev = "Commercial",
                           year = rep(years, times = length(areas)),
                           sex = rep(2, times = length(years)*length(areas)),
                           area = rep(areas, each = length(years))
      )

      join <- dplyr::right_join(freq, labels,
                                by = c("species_common_name", "survey_abbrev",
                                       "year", "sex", "area"),
                                keep = FALSE)

      join <- join %>%
        dplyr::select(-sex)

      lengths <- join %>%
        dplyr::arrange(species_common_name, survey_abbrev, area, year)

      # Sexed and unsexed totaled
    } else if (is.null(areas)) {

    labels <- data.frame(species_common_name = unique(dat$species_common_name),
                         survey_abbrev = "Commercial",
                         year = years,
                         sex = rep(2, each = length(years)),
                         area = rep("Total", each = length(years))
                         )

    join <- dplyr::right_join(freq, labels,
                                 by = c("species_common_name", "survey_abbrev",
                                        "year", "sex", "area"),
                                 keep = FALSE)

    join <- join %>%
      dplyr::select(-sex)

    lengths <- join %>%
      dplyr::arrange(species_common_name, survey_abbrev, area, year)

    }
  }

  if (remove_unsexed == TRUE) {

    # Sexed ONLY by area
    if (!is.null(areas)){

      labels <- data.frame(species_common_name = unique(dat$species_common_name),
                           survey_abbrev = "Commercial",
                           year = rep(years, each = 2, times = length(areas)),
                           sex = rep(c("M", "F"), times = length(years)*length(areas)),
                           area = rep(areas, each = length(years)*2)
      )

      join <- dplyr::right_join(freq, labels,
                                by = c("species_common_name", "survey_abbrev",
                                       "year", "sex", "area"),
                                keep = FALSE)
      lengths <- join %>%
        dplyr::arrange(species_common_name, survey_abbrev, area, year, sex)

      # Sexed ONLY totaled
    } else if (is.null(areas)) {

      labels <- data.frame(species_common_name = unique(dat$species_common_name),
                           survey_abbrev = "Commercial",
                           year = rep(years, each = 2),
                           sex = rep(c("M", "F"), times = length(years)),
                           area = rep("Total", each = length(years)*2)
      )

      join <- dplyr::right_join(freq, labels,
                                by = c("species_common_name", "survey_abbrev",
                                       "year", "sex", "area"),
                                keep = FALSE)
      lengths <- join %>%
        dplyr::arrange(species_common_name, survey_abbrev, area, year, sex)

    }
  }

  return(lengths)
}



tidy_ages_comm_raw <- function(dat,
                               years = NULL,
                               areas = NULL,
                               area_grep_pattern = "*",
                               sorted,
                               usability_codes = c(0, 1, 2, 6),
                               age_length = "age",
                               sample_type = "commercial",
                               frequency_type = "raw",
                               remove_unsexed = TRUE,
                               ...) {

  # -------------------------------------------
  # Filter down data (basics):

  spp <- unique(dat$species_common_name)

  if (remove_unsexed) {
    dat <- dat %>%
      dplyr::filter(sex %in% c(1, 2))
  }

  dat$sex <- dplyr::case_when(
    dat$sex == 1 ~ "M",
    dat$sex == 2 ~ "F"
  )

  dat <- dat %>%
    dplyr::filter(!is.na(year))

  if (is.null(years)) years <- seq(min(dat$year), max(dat$year), by = 1)

  dat <- dat %>%
    dplyr::filter(year >= min(years), year <= max(years))

  # -------------------------------------------
  # Filter down usability codes:
  if (!is.null(usability_codes)) {
    dat <- dat %>%
      dplyr::filter(usability_code %in% usability_codes)
  }

  # Filter down sampling description (sorted vs unsorted, unknown excluded):
  if (sorted) {
    dat <- dat %>%
      dplyr::filter(sampling_desc == "KEEPERS" | sampling_desc == "DISCARDS")
  } else if (!sorted) {
    dat <- dat %>%
      dplyr::filter(sampling_desc == "UNSORTED")
  }

  # -------------------------------------------
  # Filter down data (commercial):
  if (sample_type == "commercial") {
    if (nrow(dat) == 0) {
      if (!is.null(areas)) {
        blank_areas <- data.frame(species_common_name = spp,
                                  survey_abbrev = "Commercial",
                                  area = rep(areas, each = length(years)*2),
                                  year = rep(years, each = 2, times = length(areas)),
                                  sex = rep(c("M", "F"), times = length(years)*length(areas)),
                                  age = NA,
                                  proportion = NA,
                                  total = NA
        )

        return(blank_areas)

      } else {
        blank_total <- data.frame(species_common_name = spp,
                            survey_abbrev = "Commercial",
                            area = rep("Total", each = length(years)*2),
                            year = rep(years, each = 2),
                            sex = rep(c("M", "F"), times = length(years)),
                            age = NA,
                            proportion = NA,
                            total = NA
        )
        return(blank_total)
      }
    }

    dat$survey_abbrev <- "Commercial"

    pbs_areas <- gfplot::pbs_areas[grep(
      area_grep_pattern,
      gfplot::pbs_areas$major_stat_area_description
    ), , drop = FALSE]

    dat <- dplyr::semi_join(dat, pbs_areas, by = "major_stat_area_code")
  }

  # Assign areas (or total)
  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat <- dat %>%
      dplyr::mutate(area = "Total")
  }

  # -------------------------------------------
  # Filter down data (ages):
  if (age_length == "age") {

    dat <- dat %>%
      dplyr::filter(!is.na(age))

    # If no data, create a blank data frame
    if (nrow(dat) == 0) {
      if (!is.null(areas)) {
        blank_areas <- data.frame(species_common_name = spp,
                                  survey_abbrev = "Commercial",
                                  area = rep(areas, each = length(years)*2),
                                  year = rep(years, each = 2, times = length(areas)),
                                  sex = rep(c("M", "F"), times = length(years)*length(areas)),
                                  age = NA,
                                  proportion = NA,
                                  total = NA
        )

        return(blank_areas)

        # If no data, create a blank data frame
      } else {
        blank_total <- data.frame(species_common_name = spp,
                                  survey_abbrev = "Commercial",
                                  area = rep("Total", each = length(years)*2),
                                  year = rep(years, each = 2),
                                  sex = rep(c("M", "F"), times = length(years)),
                                  age = NA,
                                  proportion = NA,
                                  total = NA
        )
        return(blank_total)
      }
    }
  }

  # Remove duplicate specimens
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical

  # -------------------------------------------
  # Retain only necessary columns:
  if (frequency_type == "raw") {
    dat <- dat %>%
      dplyr::select(species_common_name, survey_abbrev, area, year, sex, age)
  }

  # -------------------------------------------
  # Calculate the actual age or length frequencies:

  # -------------------------------------------
  # Raw (commercial or survey):
  if (age_length == "age" && frequency_type == "raw") {
    freq <- dat %>%
      dplyr::group_by(species_common_name, area, year, age, sex, survey_abbrev) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::group_by(year, survey_abbrev) %>%
      dplyr::mutate(proportion = n / sum(n)) %>%
      dplyr::group_by(survey_abbrev) %>%
      dplyr::mutate(proportion = proportion / max(proportion)) %>%
      dplyr::select(-n) %>%
      dplyr::ungroup()
  }

  # -------------------------------------------
  # Join in the counts for labels:
  if (age_length == "age") {
    counts <- dat %>%
      dplyr::group_by(year, species_common_name, survey_abbrev, area) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::ungroup()

    freq <- dplyr::left_join(freq, counts,
                             by = c("species_common_name", "year", "survey_abbrev", "area")
    )

    freq <- freq %>%
      dplyr::select(species_common_name, survey_abbrev, area, year, sex, age, proportion, total)
  }

  # -------------------------------------------
  # Fill in blanks with NAs

  # For areas
  if (!is.null(areas)){

    labels <- data.frame(species_common_name = unique(dat$species_common_name),
                         survey_abbrev = "Commercial",
                         year = rep(years, each = 2, times = length(areas)),
                         sex = rep(c("M", "F"), times = length(years)*length(areas)),
                         area = rep(areas, each = length(years)*2)
    )

    join <- dplyr::right_join(freq, labels,
                              by = c("species_common_name", "survey_abbrev",
                                     "year", "sex", "area"),
                              keep = FALSE)
    ages <- join %>%
      dplyr::arrange(species_common_name, survey_abbrev, area, year, sex)

    # For totals
  } else if (is.null(areas)) {

    labels <- data.frame(species_common_name = unique(dat$species_common_name),
                         survey_abbrev = "Commercial",
                         year = rep(years, each = 2),
                         sex = rep(c("M", "F"), times = length(years)),
                         area = rep("Total", each = length(years)*2)
    )

    join <- dplyr::right_join(freq, labels,
                              by = c("species_common_name", "survey_abbrev",
                                     "year", "sex", "area"),
                              keep = FALSE)
    ages <- join %>%
      dplyr::arrange(species_common_name, survey_abbrev, area, year, sex)

  }

}


