# An R script to arrange and sitch together plots for page 2

plot_layout_pg_2 <- function(spp,
                             years = 1996:2022,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "report-rmd", "figs"),
                             fl_type = ".png",
                             width = 450,
                             height = 350,
                             units = "mm",
                             dpi = 200,
                             debug = FALSE
) {

  # Read in data ---------------------------------------------------------------

  data <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))


  comm_samples <- data$commercial_samples

  if (spp == "sablefish") {
    sablefish <- sablefish_heads(fl_path = paste0(here::here("data")), fl_name = "sablefish_heads.csv")

    comm_samples <- dplyr::bind_rows(comm_samples, sablefish)
  }


  # Commercial counts plot -----------------------------------------------------

  comm_samples_sort <- subset(comm_samples,
                              comm_samples$sampling_desc == "DISCARDS" | comm_samples$sampling_desc == "KEEPERS")
  comm_samples_unsort <- subset(comm_samples,
                                comm_samples$sampling_desc == "UNSORTED")

  counts_sort <- tidy_commercial_counts(comm_samples_sort,
                                        years = years)

  counts_unsort <- tidy_commercial_counts(comm_samples_unsort,
                                          years = years)

  p3_sort <- plot_commercial_counts(counts_sort,
                                    years = years,
                                    sorted = TRUE)

  p3_unsort <- plot_commercial_counts(counts_unsort,
                                      years = years,
                                      sorted = FALSE)


  # Arranging plots  -----------------------------------------------------------

  p_counts <- cowplot::plot_grid(p3_unsort, p3_sort,
                                 ncol = 2, nrow = 1, align = "h", axis = "tb")

  # Save plot ------------------------------------------------------------------

  # Plot name
  plot_name <- paste0(spp, "-pg-2")

  # Save ggplot
  ggplot2::ggsave(
    paste0(fl_path_store,"/", plot_name, fl_type),
    plot = p_counts,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )

}
