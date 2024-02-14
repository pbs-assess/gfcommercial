

sablefish_heads <- function ( ) {

  sablefish <- read.csv(paste0(here::here("data"), "/", "sablefish_heads.csv"))

  # Adjust column names
  names(sablefish) <- tolower(names(sablefish))
  names(sablefish)[names(sablefish) == "lengthpred"] <- "length"
  names(sablefish)[names(sablefish) == "specimen"] <- "specimen_id"

  sablefish <- sablefish %>%
    dplyr::select(-interorbital)

  sablefish <- sablefish %>%
    dplyr::mutate(year = lubridate::year(trip_start_date),
                  sampling_desc = "UNSORTED",
                  species_common_name = "sablefish",
                  trip_start_date = as.POSIXct(trip_start_date),
                  age = as.numeric(age),
                  weight = as.numeric(weight),
                  major_stat_area_code = as.character(major_stat_area_code),
                  trip_sub_type_desc = "NON - OBSERVED DOMESTIC",
                  maturity_code = 0,
                  length = round_any(length, accuracy = 10)*0.1,
                  usability_code = 0
                  )

}

