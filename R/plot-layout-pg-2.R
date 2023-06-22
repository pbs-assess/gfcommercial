# An R script to arrange and sitch together plots for page 1

plot_layout_pg_2 <- function(spp,
                             years = 1996:2021,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 300,
                             height = 450,
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

  # Representativeness plot ----------------------------------------------------

  cumulative_props <- tidy_cumulative_props(comm_catch,
                                            comm_samples,
                                            years = years
  )

  # Make plot ------------------------------------------------------------------
  p_gear <- cowplot::plot_grid(p1, p2,
                               ncol = 2, nrow = 1, align = "h", axis = "tb")

  p_rep <- plot_cumulative_counts(cumulative_props)

  p <- cowplot::plot_grid(p_gear, p_rep,
                          ncol = 1, nrow = 2,
                          rel_heights = c(1, 1.5))
  # Save plot ------------------------------------------------------------------

  # Plot name
  plot_name <- paste0(spp, "-pg-2")

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
