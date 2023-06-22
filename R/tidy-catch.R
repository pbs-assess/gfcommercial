# Modified tidy catch function from gfplot - Sean Anderson

# Tidy catch function ----------------------------------------------------------
# a function to tidy the catch data by gear type and by area or total

my_tidy_catch <- function(dat,
                          areas = NULL,
                          ...) {

  # Assign areas (or total)
  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat$area <- "Total"
  }

  # Summarize catch data
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

  # Rename gear types
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

  # Rename gear for all discards as 'discarded'
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
                      gear = forcats::fct_relevel(gear, relevel_gears)
  )

  all_catch <- group_by(all_catch, year, species_common_name, area, gear) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()

  all_catch
}


# Total catch function ---------------------------------------------------------
# a function to amalgamate the tidied catch data by area and total

catch_total <- function(dat, years = NULL, ...) {

  # Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")


  # Tidy catch data by areas and for total and combine the data frames
  catch_areas <- my_tidy_catch(dat,
                               areas = c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B"),
                               ...)
  catch_total <- my_tidy_catch(dat,
                               areas = NULL,
                               ...)
  catch <- dplyr::bind_rows(catch_total, catch_areas)

  catch <- catch %>%
    dplyr::mutate(area = factor(area, levels = area_levels))

  # Define years
  if (is.null(years)) {
    years <- seq(min(catch$year, na.rm = TRUE), max(catch$year, na.rm = TRUE), 1L)
  }

  # Filter by years
  catch <- catch %>%
    dplyr::filter(year %in% years)

  # Fill in missing values with NA
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
