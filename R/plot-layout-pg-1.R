# An R script to arrange and sitch together plots for page 1

plot_layout_pg_1 <- function(spp,
                             years = 1996:2022,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "report-rmd", "figs"),
                             fl_type = ".png",
                             width = 300,
                             height = 200,
                             units = "mm",
                             dpi = 600,
                             debug = FALSE
) {

  # Read in data ---------------------------------------------------------------

  data <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  # Commercial catch plot ------------------------------------------------------

  comm_catch <- data$catch

  catch <- catch_total(comm_catch,
                       years = years)

  p1 <- plot_catches(catch, years = years)

  # Commercial samples by gear type --------------------------------------------

  comm_samples <- data$commercial_samples

  samples <- samples_total(comm_samples,
                           years = years)

  p2 <- plot_samples(samples, years = years)

  # Make plot ------------------------------------------------------------------
  p <- cowplot::plot_grid(p1, p2,
                          ncol = 2, nrow = 1, align = "h", axis = "tb")

  # Save plot ------------------------------------------------------------------

  # Plot name
  plot_name <- paste0(spp, "-pg-1")

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
