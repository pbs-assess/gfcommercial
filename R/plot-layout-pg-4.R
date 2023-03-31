plot_layout_pg_4 <- function(spp,
                             years = 1996:2021,
                             fl_path_data = here::here("data-cache"), # change to wherever the data cache is located
                             fl_path_store = here::here("report", "figs"),
                             fl_type = ".png",
                             width = 275,
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

  p <- plot_cumulative_counts(cumulative_props)


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


