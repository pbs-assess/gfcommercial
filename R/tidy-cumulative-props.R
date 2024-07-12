# A function to merge the catch and samples cumulative proportion data frames

tidy_cumulative_props <- function (catch_data,
                                   samples_data,
                                   FE_dat,
                                   years = NULL) {

  # Tidy catch and samples data ------------------------------------------------

  # Tidy commercial catch data
  catch_counts <- tidy_cumulative_counts(catch_data,
                                         years = years,
                                         variable = "catch")

  # Tidy commercial sample data
  sample_counts <- tidy_cumulative_counts(samples_data,
                                          years = years,
                                          variable = "samples")

  spatial_counts <- tidy_cumulative_counts(samples_data,
                                           FE_dat,
                                           years = years,
                                           variable = "spatial")

  # Merge catch and samples data -----------------------------------------------

  cumulative_props <- dplyr::left_join(catch_counts, sample_counts,
                                       by = c("species_common_name",
                                              "area",
                                              "year",
                                              "week"),
                                       keep = FALSE)

  cumulative_props <- dplyr::left_join(cumulative_props, spatial_counts,
                                       by = c("species_common_name",
                                              "area",
                                              "year",
                                              "week"),
                                       keep = FALSE)

  # Rename merged columns ------------------------------------------------------

  names(cumulative_props)[names(cumulative_props) == "proportion.x"] <- "catch_prop"
  names(cumulative_props)[names(cumulative_props) == "n.x"] <- "n_catch"
  names(cumulative_props)[names(cumulative_props) == "proportion.y"] <- "samples_prop"
  names(cumulative_props)[names(cumulative_props) == "n.y"] <- "n_samples"
  names(cumulative_props)[names(cumulative_props) == "proportion"] <- "spatial_prop"
  names(cumulative_props)[names(cumulative_props) == "n"] <- "n_spatial"

  # Remove unnecessary columns  ------------------------------------------------

  cumulative_props <- cumulative_props %>%
    dplyr::select(species_common_name,
                  area,
                  year,
                  week,
                  catch_prop,
                  n_catch,
                  samples_prop,
                  n_samples,
                  spatial_prop,
                  n_spatial)

  # Return proportions  --------------------------------------------------------

  return(cumulative_props)
}
