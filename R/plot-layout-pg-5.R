plot_layout_pg_5 <- function(spp,
                             years = 1996:2021,
                             sample_type = "commercial",
                             bin_size = 2,
                             fl_path_data = here::here("data-cache"), # change to wherever the data cache is located
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 175,
                             height = 150,
                             units = "mm",
                             dpi = 300
) {

  # Read in data  --------------------------------------------------------------

  dat <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- dat$commercial_samples

  # Lengths plot ---------------------------------------------------------------

  lengths <- tidy_lengths_by_areas_raw(comm_samples,
                                       sample_type = sample_type,
                                       bin_size = bin_size,
                                       year_range = c(min(years), max(years))
  )

  p <- plot_commercial_lengths(lengths,
                               year_range = c(min(years), max(years))
  )

  # Save plot ------------------------------------------------------------------

  # Plot values
  plot_name <- paste0(spp, "-pg-5")

  # Save ggplot
  ggplot2::ggsave(paste0(fl_path_store,"/", plot_name, fl_type),
                  plot = p,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi
  )

}

