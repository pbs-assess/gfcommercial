# An R script to arrange and sitch together plots for page 4

plot_layout_pg_4 <- function(spp,
                             years = 1996:2022,
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

  # Get Sablefish lengths from head measurments
  if (spp == "sablefish") {
    sablefish <- sablefish_heads()
    comm_samples <- dplyr::bind_rows(comm_samples, sablefish)
  }

  # [Halibut] Change discarded bottom trawl specimens to unsorted
  if (spp == "pacific-halibut") {
    comm_samples <- comm_samples |>
      dplyr::mutate(sampling_desc = ifelse(sampling_desc == "UNKNOWN", "DISCARDS", sampling_desc),
                    sampling_desc = ifelse(sampling_desc == "DISCARDS" & gear_desc == "BOTTOM TRAWL", "UNSORTED", sampling_desc))
  }

  # Filter outliers and determine bin width
  lengths_no_outliers <- comm_samples |>
    dplyr::filter(!length %in% find_length_outliers(length))

  if (spp %in% c("redstripe-rockfish", "walleye-pollock", "yellowmouth-rockfish", "yelloweye-rockfish")) {
    bin_width <- 2
  } else if (spp == "pacific-cod") {
    bin_width <- 4
  } else {
    bin_width <- round_down_even((diff(quantile(lengths_no_outliers$length, na.rm = TRUE, probs = c(0, 1))) / 20)*0.85, base = 1)
  }

  # Begin plot making

  if (spp %in% c("longspine-thornyhead", "pacific-spiny-dogfish", "shortspine-thornyhead")) {

    fork_l <- lengths_no_outliers |>
      dplyr::filter(length_type == "fork_length")

    total_l <- lengths_no_outliers |>
      dplyr::filter(length_type == "total_length")

    # Length frequency plot (FORK) ---------------------------------------------

    fork_l_sex <- tidy_lengths(fork_l,
                               years = years,
                               total = FALSE,
                               sorted = FALSE,
                               bin_size = bin_width
    )

    fork_l_total <- tidy_lengths(fork_l,
                                 years = years,
                                 total = TRUE,
                                 sorted = FALSE,
                                 bin_size = bin_width
    )

    fork_l_total <- fork_l_total %>%
      dplyr::mutate(sex = "Total")

    fork_l_sorted <- tidy_lengths(fork_l,
                                  years = years,
                                  total = TRUE,
                                  sorted = TRUE,
                                  bin_size = bin_width
    )

    #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

    p1 <- plot_commercial_lengths(fork_l_sex, fork_l_total, fork_l_sorted,
                                  year_range = c(min(years), max(years)),
                                  bin_size = bin_width
    )

    # Length frequency plot (TOTAL) --------------------------------------------

    total_l_sex <- tidy_lengths(total_l,
                                years = years,
                                total = FALSE,
                                sorted = FALSE,
                                bin_size = bin_width
    )

    total_l_total <- tidy_lengths(total_l,
                                  years = years,
                                  total = TRUE,
                                  sorted = FALSE,
                                  bin_size = bin_width
    )

    total_l_total <- total_l_total %>%
      dplyr::mutate(sex = "Total")

    total_l_sorted <- tidy_lengths(total_l,
                                   years = years,
                                   total = TRUE,
                                   sorted = TRUE,
                                   bin_size = bin_width
    )

    #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

    p2 <- plot_commercial_lengths(total_l_sex, total_l_total, total_l_sorted,
                                  year_range = c(min(years), max(years)),
                                  bin_size = bin_width
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

    lengths_sex <- tidy_lengths(lengths_no_outliers,
                                years = years,
                                total = FALSE,
                                sorted = FALSE,
                                bin_size = bin_width
    )

    lengths_total <- tidy_lengths(lengths_no_outliers,
                                  years = years,
                                  total = TRUE,
                                  sorted = FALSE,
                                  bin_size = bin_width
    )

    lengths_total <- lengths_total %>%
      dplyr::mutate(sex = "Total")

    lengths_sorted <- tidy_lengths(lengths_no_outliers,
                                   years = years,
                                   total = TRUE,
                                   sorted = TRUE,
                                   bin_size = bin_width
    )

    #lengths <- dplyr::bind_rows(lengths_sex, lengths_total)

    p3 <- plot_commercial_lengths(lengths_sex, lengths_total, lengths_sorted,
                                  year_range = c(min(years), max(years)),
                                  bin_size = bin_width
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
