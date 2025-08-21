# New tidy_lengths function
tidy_lengths <- function (dat,
                          years = NULL,
                          total,
                          ...) {

area_grep_pattern <-  "^3C|^3D|^5A|^5B|^5C|^5D|^5E|^4B"
area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

# Return a data frame of the length data split by sex
if (total == FALSE) {

  length_sex_area <- tidy_lengths_comm_raw(dat,
                                           years = years,
                                           areas = c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B"),
                                           area_grep_pattern = area_grep_pattern,
                                           remove_unsexed = TRUE,
                                           ...)

  length_sex_total <- tidy_lengths_comm_raw(dat,
                                            years = years,
                                            areas = NULL,
                                            area_grep_pattern = area_grep_pattern,
                                            remove_unsexed = TRUE,
                                            ...)

  lengths_sex <- dplyr::bind_rows(length_sex_area, length_sex_total)

  lengths_sex$area <- factor(lengths_sex$area, levels = area_levels)

  lengths_sex <- lengths_sex %>%
    dplyr::arrange(area)

  return(lengths_sex)

 }

# Return a data frame of the length data totaled, including unsexed specimens
if (total == TRUE) {

  length_totals_area <- tidy_lengths_comm_raw(dat,
                                              years = years,
                                              areas = c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B"),
                                              area_grep_pattern = area_grep_pattern,
                                              remove_unsexed = FALSE,
                                              ...
  )

  length_totals_total <- tidy_lengths_comm_raw(dat,
                                               years = years,
                                               areas = NULL,
                                               area_grep_pattern = area_grep_pattern,
                                               remove_unsexed = FALSE,
                                               ...
  )

  lengths_total <- dplyr::bind_rows(length_totals_area, length_totals_total)

  lengths_total$area <- factor(lengths_total$area, levels = area_levels)

  lengths_total <- lengths_total %>%
    dplyr::arrange(area)

  return(lengths_total)

  }

}

#-------------------------------------------
# New tidy_ages function

tidy_ages <- function (dat,
                       years= NULL,
                       ...) {

  area_grep_pattern <-  "^3C|^3D|^5A|^5B|^5C|^5D|^5E|^4B"
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  # Tidy age data by area
  ages_area <- tidy_ages_comm_raw(dat,
                                  years = years,
                                  areas = c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B"),
                                  area_grep_pattern = area_grep_pattern,
                                  ...)

  # Tidy age data totaled across all areas
  ages_total <- tidy_ages_comm_raw(dat,
                                   years = years,
                                   areas = NULL,
                                   area_grep_pattern = area_grep_pattern,
                                   ...)

  ages <- dplyr::bind_rows(ages_area, ages_total)

  ages$area <- factor(ages$area, levels = area_levels)

  ages <- ages %>%
    dplyr::arrange(area)

  return(ages)

}
