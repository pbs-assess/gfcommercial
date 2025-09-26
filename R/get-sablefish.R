

sablefish_heads <- function (fl_path = paste0(here::here("data")),
                             fl_name = "sablefish_heads.csv") {

  sablefish <- read.csv(paste0(fl_path, "/", fl_name))

  # Adjust column names
  names(sablefish) <- tolower(names(sablefish))
  names(sablefish)[names(sablefish) == "lengthpred"] <- "length"
  names(sablefish)[names(sablefish) == "specimen"] <- "specimen_id"

  sablefish <- sablefish %>%
    dplyr::select(-interorbital)

  sablefish <- sablefish %>%
    dplyr::mutate(trip_start_date = as.POSIXct(trip_start_date, format = "%m/%d/%Y"),
                  species_common_name = tolower(species_common_name),
                  age = as.numeric(age),
                  weight = as.numeric(weight),
                  maturity_code = as.numeric(maturity_code),
                  major_stat_area_code = as.character(major_stat_area_code),
                  length = round_any(length, accuracy = 10)*0.1,
                  length_type = "fork_length"
                  )

}

