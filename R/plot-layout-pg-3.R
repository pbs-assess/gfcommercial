# An R script to arrange and sitch together plots for page 1

plot_layout_pg_3 <- function(spp,
                             years = 1996:2021,
                             sample_type = "commercial",
                             fl_path_data = here::here("data-cache"), # change to wherever the data cache is located
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 300,
                             height = 250,
                             units = "mm",
                             dpi = 300
) {

  # Read in data  --------------------------------------------------------------

  dat <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- dat$commercial_samples

  # Commercial ages plot -------------------------------------------------------

  ages <- tidy_ages_by_areas_raw(comm_samples,
                                 sample_type = sample_type,
                                 year_range = c(min(years), max(years))
  )

  p1 <- plot_commercial_ages(ages,
                             sex = "M",
                             year_range = c(min(years), max(years))
  )

  p2 <- plot_commercial_ages(ages,
                             sex = "F",
                             year_range = c(min(years), max(years))
  )


  # Arrange plots --------------------------------------------------------------

  p <- ggpubr::ggarrange(p1, p2,
                         ncol = 1, nrow = 2)

  # Save plot ------------------------------------------------------------------

  # Plot values
  plot_name <- paste0(spp, "-pg-3")

  # Save ggplot
  ggplot2::ggsave(paste0(fl_path_store,"/", plot_name, fl_type),
                  plot = p,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi
  )

}
