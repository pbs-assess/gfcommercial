# An R script to arrange and sitch together plots for page 3

plot_layout_pg_3 <- function(spp,
                             years = 2000:2022,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "report-rmd", "figs"),
                             fl_type = ".png",
                             width = 300,
                             height = 275,
                             units = "mm",
                             dpi = 200,
                             debug = FALSE
) {

  # Read in data ---------------------------------------------------------------

  data <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  FE_dat <- read.csv(paste0(here::here("data"), "/", "fishing_event.csv"))

  # Commercial catch plot ------------------------------------------------------

  comm_catch <- data$catch

  # Commercial samples by gear type --------------------------------------------

  comm_samples <- data$commercial_samples

  if (spp == "sablefish") {
    sablefish <- sablefish_heads()

    comm_samples <- dplyr::bind_rows(comm_samples, sablefish)
  }

  comm_samples_unsort <- dplyr::filter(comm_samples, sampling_desc == "UNSORTED")

  # Representativeness plot ----------------------------------------------------

  cumulative_props <- tidy_cumulative_props(comm_catch,
                                            comm_samples_unsort,
                                            FE_dat,
                                            years = years
  )

  # Make plot ------------------------------------------------------------------

  p <- plot_representativeness(cumulative_props)

  # Save plot ------------------------------------------------------------------

  # Plot name
  plot_name <- paste0(spp, "-pg-3.1")

  # Save ggplot
  ggplot2::ggsave(
    paste0(fl_path_store,"/", plot_name, fl_type),
    plot = p,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )

}
