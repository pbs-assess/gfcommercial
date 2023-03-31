# Modified tidy catch function from gfplot
# credit to Sean Anderson


# Set fishing year (not needed) ------------------------------------------------
set_fish_year <- function(dat,
                             month_fishing_starts = 1,
                             day_fishing_starts = 1,
                             yr_col = "year",
                             date_col = c("best_date", "trip_start_date"),
                             ...) {
  stopifnot(yr_col %in% names(dat))
  stopifnot(date_col %in% names(dat))
  stopifnot(month_fishing_starts %in% 1:12)
  stopifnot(day_fishing_starts %in% 1:31)
  if (month_fishing_starts == 2 && day_fishing_starts > 28) {
    stop("day_fishing_starts must be 28 or less for February", call. = FALSE)
  }
  if (month_fishing_starts %in% c(4, 6, 9, 11) && day_fishing_starts > 30) {
    stop("day_fishing_starts must be 30 or less for April, June, September, or November", call. = FALSE)
  }

  yr_col_sym <- sym(yr_col)
  date_col_sym <- sym(date_col)

  dat %>%
    mutate(
      day_of_year = lubridate::yday(!!date_col_sym),
      cutoff_day = lubridate::yday(lubridate::ymd(paste0(!!yr_col_sym, "-", month_fishing_starts, "-", day_fishing_starts))),
      !!yr_col_sym := ifelse(day_of_year < cutoff_day, year - 1, year)
    ) %>%
    select(-day_of_year, -cutoff_day)
}


# Assign areas -----------------------------------------------------------------

assign_areas <- function(major_stat_area_description,
                         area_regex = c("3[CD]+", "5[AB]+", "5[CDE]+")) {
  out <- rep(NA, length(major_stat_area_description))
  for (i in seq_along(area_regex)) {
    out[grepl(area_regex[i], major_stat_area_description)] <-
      gsub("\\^|\\[|\\]|\\+", "", area_regex[i])
  }
  out
}


# Tidy catch -------------------------------------------------------------------

my_tidy_catch <- function(dat,
                          areas = NULL,
                          ...) {

  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat$area <- "Total"
  }

  dat <- filter(dat, !is.na(species_common_name), !is.na(year)) %>%
    group_by(year, species_common_name, gear, area) %>%
    summarise(
      landed_kg = sum(landed_kg, na.rm = TRUE),
      discarded_kg = sum(discarded_kg, na.rm = TRUE),
      landed_pcs = sum(landed_pcs, na.rm = TRUE),
      discarded_pcs = sum(discarded_pcs, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(species_common_name, year)

  # Note that discarded_kg only includes trawl discards from 1996+ and
  # trap/hook and line discards from 2006+
  catches <- mutate(dat,
                    gear = dplyr::recode(gear,
                                         UNKNOWN = "Unknown/trawl",
                                         `BOTTOM TRAWL` = "Bottom trawl",
                                         `HOOK AND LINE` = "Hook and line",
                                         `LONGLINE` = "Hook and line",
                                         `MIDWATER TRAWL` = "Midwater trawl",
                                         `TRAP` = "Trap",
                                         `UNKNOWN TRAWL` = "Unknown/trawl"
                    )
  ) %>%
    select(year, area, species_common_name, gear, landed_kg, discarded_kg)

  cm <- reshape2::melt(catches,
                       id.vars = c("year", "species_common_name", "area", "gear")
  )

  landings <- filter(cm, variable %in% c("landed_kg"))
  discards <- filter(cm, variable %in% c("discarded_kg"))

  landings$gear <- as.character(landings$gear)
  discards$gear <- as.character(discards$gear)
  discards$gear <- "Discarded"

  all_catch <- bind_rows(landings, discards)
  # Make a vector of new levels based on what is actually in the data
  # Without this, pre-filtered catch data may cause an error if some
  # of the gear types are missing in the table.
  # The levels will follow the order of `all_gear_ordered` for present
  # gear types, missing ones are not included in the levels
  gears_present <- unique(all_catch$gear)
  all_gear_ordered <- c("Bottom trawl",
                        "Midwater trawl",
                        "Hook and line",
                        "Trap",
                        "Unknown/trawl",
                        "Discarded")
  relevel_gears <- gears_present[order(match(gears_present, all_gear_ordered))]
  all_catch <- mutate(all_catch,
                      gear = forcats::fct_relevel(
                        gear, relevel_gears
                      )
  )

  all_catch <- group_by(all_catch, year, species_common_name, area, gear) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()

  all_catch
}


# Total catch ------------------------------------------------------------------

catch_total <- function(dat, years = NULL, ...) {

  # Define area factor levels --------------------------------------------------

  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")


  # Create a 'Total' area ------------------------------------------------------

  catch_areas <- my_tidy_catch(dat,
                               areas = c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B"),
                               ...)
  catch_total <- my_tidy_catch(dat,
                               areas = NULL,
                               ...)
  catch <- dplyr::bind_rows(catch_total, catch_areas)

  catch <- catch %>%
    dplyr::mutate(area = factor(area, levels = area_levels))

  # Define years ---------------------------------------------------------------

  if (is.null(years)) {
    years <- seq(min(catch$year, na.rm = TRUE), max(catch$year, na.rm = TRUE), 1L)
  }

  # Filter by years ------------------------------------------------------------

  catch <- catch %>%
    dplyr::filter(year %in% years)

  # Fill in missing values with NA ---------------------------------------------

  gear <- c("Bottom trawl",
            "Midwater trawl",
            "Hook and line",
            "Trap",
            "Unknown/trawl",
            "Discarded")

  labels <- data.frame(
    species_common_name = unique(catch$species_common_name),
    area = rep(area_levels,
               each = length(years)*length(gear)),
    year = rep(years,
               each = length(gear),
               times = length(area_levels)),
    gear = rep(gear,
               times = length(area_levels)*length(years))
  )

  catch_all <- dplyr::right_join(catch,
                                 labels,
                                 by = c("species_common_name", "area", "year", "gear"),
                                 keep = FALSE)

  catch_all <- catch_all %>%
    dplyr::arrange(area, year, gear)

  catch_all$area <- factor(catch_all$area, levels = area_levels)

  catch_all <- catch_all %>%
    dplyr::arrange(area)

  # Return catch data frame ----------------------------------------------------

  return(catch)

}
