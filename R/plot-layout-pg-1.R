# An R script to arrange and sitch together plots for page 1

plot_layout_pg_1 <- function(spp,
                             years = 2003:2021,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 190,
                             height = 120,
                             units = "mm",
                             dpi = 300
) {

  # Commercial counts plot -----------------------------------------------------

  dat <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- dat$commercial_samples

  counts <- tidy_commercial_counts(
    comm_samples,
    years = years
  )

  p1 <- plot_commercial_counts(counts,
                               years = years)
  p1

  # Plot values
  plot_name <- paste0(spp, "-pg-1")

  # Save ggplot
  ggplot2::ggsave(
    paste0(fl_path_store,"/", plot_name, fl_type),
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )

}
