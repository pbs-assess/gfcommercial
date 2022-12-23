# A function to merge the catch and samples cumulative proportion data frames

tidy_cumulative_props <- function (catch_data,
                                   samples_data,
                                   years = NULL) {
  
  # Tidy catch and samples data ------------------------------------------------
  
  # Tidy commercial catch data
  catch_counts <- tidy_cumulative_counts(catch_data,
                                         years = years, # Luke how would you write this so it is clear and not confusing about which 'years' is getting passed
                                         variable = "catch")
  
  # Tidy commercial sample data
  sample_counts <- tidy_cumulative_counts(samples_data,
                                          years = years,
                                          variable = "samples")
  
  # Merge catch and samples data -----------------------------------------------
  
  cumulative_props <- dplyr::left_join(catch_counts,
                                       sample_counts,
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
  
  # Remove unnecessary columns  ------------------------------------------------
  
  cumulative_props <- cumulative_props %>%
    dplyr::select(species_common_name,
                  area,
                  year,
                  week,
                  catch_prop,
                  n_catch,
                  samples_prop,
                  n_samples)
  
  # Return proportions  --------------------------------------------------------
  
  return(cumulative_props)
}
