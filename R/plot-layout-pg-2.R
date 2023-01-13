# An R script to arrange and sitch together plots for page 1

plot_layout_pg_2 <- function(spp,
                             years = 2003:2021,
                             sample_type = "commercial",
                             bin_size = 2,
                             fl_path_data = here::here("data-cache"), # change to wherever the data cache is located
                             fl_path_store = here::here("ms","figs"),
                             fl_type = ".png",
                             width = 190,
                             height = 275,
                             units = "mm",
                             dpi = 300
) {

  # Read in data  --------------------------------------------------------------

  dat <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- dat$commercial_samples
  catch <- dat$catch

  # Representativeness plot ----------------------------------------------------

  cumulative_props <- tidy_cumulative_props(catch,
                                            comm_samples,
                                            years = years
  )

  p1 <- plot_cumulative_counts(cumulative_props)

  # Commercial ages plot -------------------------------------------------------

  ages <- tidy_ages_by_areas_raw(comm_samples,
                                 sample_type = sample_type,
                                 year_range = c(min(years), max(years))
  )

  p2 <- plot_commercial_ages(ages,
                             year_range = c(min(years), max(years))
  )

  # Commercial lengths plot ----------------------------------------------------

  lengths <- tidy_lengths_by_areas_raw(comm_samples,
                                       sample_type = sample_type,
                                       bin_size = bin_size,
                                       year_range = c(min(years), max(years))
  )

  p3 <- plot_commercial_lengths(lengths,
                                year_range = c(min(years), max(years))
  )

  # Arrange plots --------------------------------------------------------------

  # Plot composition done using the patchwork package
  p <- p1 /
    plot_spacer() /
    p2 /
    plot_spacer() /
    p3 +
    plot_layout(heights = c(5, -0.1, 4, -0.4, 5)) &
    theme(plot.margin = grid::unit(c(-3, 0, 1, 1), "mm"))

  # Save plot ------------------------------------------------------------------

  # Plot values
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
