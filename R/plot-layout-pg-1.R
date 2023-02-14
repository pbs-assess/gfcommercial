# An R script to arrange and sitch together plots for page 1

plot_layout_pg_1 <- function(spp,
                             years = 2003:2021,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 250,
                             height = 160,
                             units = "mm",
                             dpi = 300
) {

  # Commercial counts plot -----------------------------------------------------

  dat <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

  comm_samples <- dat$commercial_samples

  comm_samples_sort <- subset(comm_samples,
                              comm_samples$sampling_desc == c("DISCARDS", "KEEPERS"))
  comm_samples_unsort <- subset(comm_samples,
                                comm_samples$sampling_desc == "UNSORTED")

  counts_sort <- tidy_commercial_counts(comm_samples_sort,
                                        years = years)

  counts_unsort <- tidy_commercial_counts(comm_samples_unsort,
                                          years = years)

  p1_sort <- plot_commercial_counts(counts_sort,
                                    years = years,
                                    title = "Sorted commercial specimen counts")

  p1_unsort <- plot_commercial_counts(counts_unsort,
                                      years = years,
                                      title = "Unsorted commercial specimen counts")

  p <- p1_sort + p1_unsort

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
