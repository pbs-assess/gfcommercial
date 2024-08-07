# An R script to arrange and sitch together plots for page 4

plot_layout_pg_4 <- function(spp,
                             years = 1996:2022,
                             bin_size = 2,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "report-rmd", "figs"),
                             fl_type = ".png",
                             width = 300,
                             height = 450,
                             units = "mm",
                             dpi = 200
) {

  # Read in data  --------------------------------------------------------------

  data <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- data$commercial_samples

  if (spp == "sablefish") {
    sablefish <- sablefish_heads()

    comm_samples <- dplyr::bind_rows(comm_samples, sablefish)
  }

  if (spp %in% c("longspine-thornyhead", "pacific-spiny-dogfish", "shortspine-thornyhead")) {

    fork_l <- comm_samples |>
      dplyr::filter(length_type == "fork_length")

    total_l <- comm_samples |>
      dplyr::filter(length_type == "total_length")

    # Length frequency plot (FORK) ---------------------------------------------

    fork_l_sex <- tidy_lengths(fork_l,
                               bin_size = bin_size,
                               years = years,
                               total = FALSE,
                               sorted = FALSE
    )

    fork_l_total <- tidy_lengths(fork_l,
                                 bin_size = bin_size,
                                 years = years,
                                 total = TRUE,
                                 sorted = FALSE
    )

    fork_l_total <- fork_l_total %>%
      dplyr::mutate(sex = "Total")

    fork_l_sorted <- tidy_lengths(fork_l,
                                  bin_size = bin_size,
                                  years = years,
                                  total = TRUE,
                                  sorted = TRUE
    )

    #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

    p1 <- plot_commercial_lengths(fork_l_sex, fork_l_total, fork_l_sorted,
                                  year_range = c(min(years), max(years))
    )

    # Length frequency plot (TOTAL) --------------------------------------------

    total_l_sex <- tidy_lengths(total_l,
                                bin_size = bin_size,
                                years = years,
                                total = FALSE,
                                sorted = FALSE
    )

    total_l_total <- tidy_lengths(total_l,
                                  bin_size = bin_size,
                                  years = years,
                                  total = TRUE,
                                  sorted = FALSE
    )

    total_l_total <- total_l_total %>%
      dplyr::mutate(sex = "Total")

    total_l_sorted <- tidy_lengths(total_l,
                                   bin_size = bin_size,
                                   years = years,
                                   total = TRUE,
                                   sorted = TRUE
    )

    #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

    p2 <- plot_commercial_lengths(total_l_sex, total_l_total, total_l_sorted,
                                  year_range = c(min(years), max(years))
    )

    # Arrange plots ------------------------------------------------------------

    p <- cowplot::plot_grid(p2, p1,
                            ncol = 1, nrow = 2,
                            na.rm = TRUE)

  } else {

    # Age frequency plot -------------------------------------------------------

    ages_unsorted <- tidy_ages(comm_samples,
                               years = years,
                               sorted = FALSE
    )

    ages_sorted <- tidy_ages(comm_samples,
                             years = years,
                             sorted = TRUE
    )

    p1 <- plot_commercial_ages(ages_unsorted,
                               ages_sorted,
                               sex = "M",
                               year_range = c(min(years), max(years))
    )

    p2 <- plot_commercial_ages(ages_unsorted,
                               ages_sorted,
                               sex = "F",
                               year_range = c(min(years), max(years))
    )


    # Length frequency plot ----------------------------------------------------

    lengths_sex <- tidy_lengths(comm_samples,
                                bin_size = bin_size,
                                years = years,
                                total = FALSE,
                                sorted = FALSE
    )

    lengths_total <- tidy_lengths(comm_samples,
                                  bin_size = bin_size,
                                  years = years,
                                  total = TRUE,
                                  sorted = FALSE
    )

    lengths_total <- lengths_total %>%
      dplyr::mutate(sex = "Total")

    lengths_sorted <- tidy_lengths(comm_samples,
                                   bin_size = bin_size,
                                   years = years,
                                   total = TRUE,
                                   sorted = TRUE
    )

    #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

    p3 <- plot_commercial_lengths(lengths_sex, lengths_total, lengths_sorted,
                                  year_range = c(min(years), max(years))
    )


    # Arrange plots ------------------------------------------------------------

    p <- cowplot::plot_grid(p1, p2, p3,
                            ncol = 1, nrow = 3, rel_heights = c(1, 1, 1.75),
                            na.rm = TRUE)

  }

  # Save plot ------------------------------------------------------------------

  # Plot values
  plot_name <- paste0(spp, "-pg-4")

  # Save ggplot
  ggplot2::ggsave(paste0(fl_path_store,"/", plot_name, fl_type),
                  plot = p,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi
  )
}
