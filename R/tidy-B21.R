# cleaning B21_samples data frame to determine data source

data_source <- function (spp,
                         years = 1996:2022,
                         fl_path_data = here::here("data-cache"),
                         fl_path_B21 = here::here("data")
                         ) {

  data <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- data$commercial_samples
  comm_samples <- comm_samples %>%
    dplyr::filter(year %in% years)

  B21 <- read.csv(paste0(fl_path_B21, "/B21_samples.csv"))

  B21 <- B21 %>%
    dplyr::mutate(TRIP_START_DATE = lubridate::mdy(TRIP_START_DATE)) %>%
    dplyr::mutate(year = lubridate::year(TRIP_START_DATE)) %>%
    dplyr::filter(year %in% years)

  SetNo <- B21 %>%
    dplyr::select(TRIP_ID, year, TRIP_START_DATE, FISHING_EVENT_ID, FE_MAJOR_LEVEL_ID) %>%
    dplyr::select(-year, -TRIP_START_DATE, -TRIP_ID) %>%
    unique()

  join <- dplyr::left_join(comm_samples, SetNo, by = dplyr::join_by("fishing_event_id" == "FISHING_EVENT_ID"), keep = FALSE)

  sources <- data.frame(
    "species" = unique(comm_samples$species_common_name),
    "dockside" = sum(length(join$FE_MAJOR_LEVEL_ID[join$FE_MAJOR_LEVEL_ID == 900]), length(join$FE_MAJOR_LEVEL_ID[join$FE_MAJOR_LEVEL_ID == 901])) / length(join$FE_MAJOR_LEVEL_ID),
    "atsea" = (length(join$FE_MAJOR_LEVEL_ID) - sum(length(join$FE_MAJOR_LEVEL_ID[join$FE_MAJOR_LEVEL_ID == 900]), length(join$FE_MAJOR_LEVEL_ID[join$FE_MAJOR_LEVEL_ID == 901]))) / length(join$FE_MAJOR_LEVEL_ID)
    )
  sources
}
