# Tidy Samples
# Modified from GF Synopsis


# Tidy samples function --------------------------------------------------------
# a function to tidy samples by gear type by area or total

my_tidy_samples <- function(dat,
                            years = NULL,
                            areas = NULL,
                            ... ) {

  # Rename gear column
  dat <- dat %>%
    dplyr::rename(gear = gear_desc)

  # Assign areas (or total)
  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat$area <- "Total"
  }

  # Remove duplicate specimens
  dat <- dat %>%
    dplyr::distinct(specimen_id, .keep_all = TRUE)

  # Recode gear names
  dat <- dat %>%
    dplyr::mutate(dat,
                  gear = dplyr::recode(gear,
                                       UNKNOWN = "Unknown/trawl",
                                       `BOTTOM TRAWL` = "Bottom trawl",
                                       `HOOK AND LINE` = "Hook and line",
                                       `LONGLINE` = "Hook and line",
                                       `MIDWATER TRAWL` = "Midwater trawl",
                                       `TRAP` = "Trap",
                                       `UNKNOWN TRAWL` = "Unknown/trawl",
                                       `HANDLINE` = "Hook and line",
                                       `JIG` = "Hook and line",
                                       `TROLL` = "Hook and line",
                                       `GILLNET` = "Unknown/trawl",
                                       `SHRIMP TRAWL` = "Unknown/trawl"
                                       )
                  )

  # Separate discards
  discards <- subset(dat, sampling_desc == "DISCARDS")
  not_discards <- subset(dat, sampling_desc != "DISCARDS")

  discards <- discards %>%
    dplyr::mutate(gear = "Discarded")

  dat_discards <- dplyr::bind_rows(discards, not_discards)

  # Summarize specimen sample data
  samples <- dplyr::filter(dat_discards, !is.na(species_common_name), !is.na(year)) %>%
    dplyr::group_by(year, species_common_name, gear, area) %>%
    dplyr::summarise(
      specimen_count = sum(!is.na(unique(specimen_id)))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(species_common_name, year) %>%
    dplyr::select(year, area, species_common_name, gear, specimen_count)


  # Make a vector of new levels based on what is actually in the data
  # Without this, pre-filtered catch data may cause an error if some
  # of the gear types are missing in the table.
  # The levels will follow the order of `all_gear_ordered` for present
  # gear types, missing ones are not included in the levels
  gears_present <- unique(samples$gear)
  all_gear_ordered <- c("Bottom trawl",
                        "Midwater trawl",
                        "Hook and line",
                        "Trap",
                        "Unknown/trawl",
                        "Discarded")
  relevel_gears <- gears_present[order(match(gears_present, all_gear_ordered))]
  all_samples <- mutate(samples,
                        gear = forcats::fct_relevel(gear, relevel_gears)
                        )

  all_samples <- all_samples %>%
    dplyr::rename(value = specimen_count)

  all_samples
}


samples_total <- function(dat, years = NULL, ...) {

  # Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  # Filter out recreational samples
  dat <- dat |>
    dplyr::filter(trip_sub_type_desc != "RECREATIONAL") |>
    dplyr::filter(gear_desc != "RECREATIONAL ROD & REEL")

  # Create a 'Total' area
  samples_areas <- my_tidy_samples(dat,
                               areas = c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B"),
                               ...)
  samples_total <- my_tidy_samples(dat,
                               areas = NULL,
                               ...)
  samples_all <- dplyr::bind_rows(samples_total, samples_areas)

  samples_all <- samples_all %>%
    dplyr::mutate(area = factor(area, levels = area_levels))

  # Define years
  if (is.null(years)) {
    years <- seq(min(samples_all$year, na.rm = TRUE), max(samples_all$year, na.rm = TRUE), 1L)
  }

  # Filter by years
  samples_all <- samples_all %>%
    dplyr::filter(year %in% years)

  # Fill in missing values with NA
  gear <- c("Bottom trawl",
           "Midwater trawl",
           "Hook and line",
           "Trap",
           "Unknown/trawl",
           "Discarded")

  labels <- data.frame(
    species_common_name = unique(samples_all$species_common_name),
    area = rep(area_levels,
               each = length(years)*length(gear)),
    year = rep(years,
               each = length(gear),
               times = length(area_levels)),
    gear = rep(gear,
               times = length(area_levels)*length(years))
  )

  samples_all <- dplyr::right_join(samples_all,
                                   labels,
                                   by = c("species_common_name", "area", "year", "gear"),
                                   keep = FALSE)

  samples_all <- samples_all %>%
    dplyr::arrange(area, year, gear)

  samples_all$area <- factor(samples_all$area, levels = area_levels)

  samples_all <- samples_all %>%
    dplyr::arrange(area)

  # Return samples data frame

  return(samples_all)

}

