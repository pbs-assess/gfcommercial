# An R script to arrange and sitch together plots for page 1

plot_layout_pg_1 <- function(spp,
                             years = 1996:2021,
                             fl_path_data = here::here("data-cache"),
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 350,
                             height = 400,
                             units = "mm",
                             dpi = 300,
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

  # Commercial counts plot -----------------------------------------------------

  comm_samples_sort <- subset(comm_samples,
                              comm_samples$sampling_desc == c("DISCARDS", "KEEPERS"))
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

  #p <- (p1 + p2) / (p3_sort + p3_unsort)

  p <- ggpubr::ggarrange(p1, p2, p3_sort, p3_unsort,
                         ncol = 2, nrow = 2)

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

#testing

plot_name <- "test"

gg_catch <- ggplot2::ggplotGrob(p1)
gg_specimens <- ggplot2::ggplotGrob(p2)
gg_sort <- ggplot2::ggplotGrob(p3_sort)
gg_unsort <- ggplot2::ggplotGrob(p3_unsort)

fg_catch <- egg::gtable_frame(gg_catch, debug = debug)
fg_specimens <- egg::gtable_frame(gg_specimens, debug = debug)
fg_sort <- egg::gtable_frame(gg_sort, debug = debug)
fg_unsort <- egg::gtable_frame(gg_unsort, debug = debug)

f_top <- egg::gtable_frame(
  gridExtra::gtable_cbind(fg_catch, fg_specimens),
  width = grid::unit(1, "null"),
  height = grid::unit(1, "null"),
  debug = debug)

f_bottom <- egg::gtable_frame(
  gridExtra::gtable_cbind(fg_sort, fg_unsort),
  width = grid::unit(1, "null"),
  height = grid::unit(1.35, "null"),
  debug = debug)

f_all <- gridExtra::gtable_rbind(f_top, f_bottom)

name <- paste0(fl_path_store,"/", plot_name, fl_type)

grDevices::png(name, width = width,
    height = height,
    res = resolution)
grid::grid.newpage()
grid::grid.draw(f_all)
dev.off()
