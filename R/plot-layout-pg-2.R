# An R script to arrange and sitch together plots for page 1

plot_layout_pg_2 <- function(spp,
                             years = 1996:2021,
                             sample_type = "commercial",
                             bin_size = 2,
                             fl_path_data = here::here("data-cache"), # change to wherever the data cache is located
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 200,
                             height = 350,
                             units = "mm",
                             dpi = 600
) {

  # Read in data  --------------------------------------------------------------

  dat <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- dat$commercial_samples

  # Age frequency plot ---------------------------------------------------------

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


  # Length frequency plot ------------------------------------------------------

  lengths_sex <- tidy_lengths_by_areas_raw(comm_samples,
                                           sample_type = sample_type,
                                           bin_size = bin_size,
                                           year_range = c(min(years), max(years)),
                                           total = FALSE
  )

  lengths_total <- tidy_lengths_by_areas_raw(comm_samples,
                                             sample_type = sample_type,
                                             bin_size = bin_size,
                                             year_range = c(min(years), max(years)),
                                             total = TRUE
  )

  lengths_total <- lengths_total %>%
    dplyr::mutate(sex = "Total")

  #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

  p3 <- plot_commercial_lengths(lengths_sex, lengths_total,
                               year_range = c(min(years), max(years))
  )


  # Arrange plots --------------------------------------------------------------

  p <- cowplot::plot_grid(p1, p2, p3,
                         ncol = 1, nrow = 3, rel_heights = c(1, 1, 1.75))

  # Save plot ------------------------------------------------------------------

  # Plot values
  plot_name <- paste0(spp, "-pg-2.4")

  # Save ggplot
  ggplot2::ggsave(paste0(fl_path_store,"/", plot_name, fl_type),
                  plot = p,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi
  )

}
