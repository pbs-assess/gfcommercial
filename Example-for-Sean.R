library(dplyr)
lapply(list.files("R", full.names = TRUE), source)

spp <- "english-sole"
years <- 1996:2021
bin_size <- 2
survey_abbrev <- "Commercial"
fl_path_data <- here::here("data-cache")

fl_path_store <- here::here("report", "figs")
fl_type <- ".png"
width <- 300
height <- 450
units <- "mm"
dpi <- 600


data <- readr::read_rds(paste0(fl_path_data, "/", spp, ".rds"))

comm_samples <- data$commercial_samples

# Example 1: original gfplot function ------------------------------------------
# Find these tidy functions in tidy-comps.R
# Luke built these functions originally, tidying the data by each area, then binding the rows all together

# Age frequency plot

ages <- tidy_ages_by_areas_raw(comm_samples,
                               year_range = c(min(years), max(years)),
                               sample_type = "commercial"

)

p1 <- plot_commercial_ages(ages,
                           sex = "M",
                           year_range = c(min(years), max(years))
)

p2 <- plot_commercial_ages(ages,
                           sex = "F",
                           year_range = c(min(years), max(years))
)


# Length frequency plot

lengths_sex <- tidy_lengths_by_areas_raw(comm_samples,
                                        bin_size = bin_size,
                                        sample_type = "commercial",
                                        year_range = c(min(years), max(years)),
                                        total = FALSE
)

lengths_total <- tidy_lengths_by_areas_raw(comm_samples,
                                           bin_size = bin_size,
                                           sample_type = "commercial",
                                           year_range = c(min(years), max(years)),
                                           total = TRUE
)

lengths_total <- lengths_total %>%
  dplyr::mutate(sex = "Total")

p3 <- plot_commercial_lengths(lengths_sex, lengths_total,
                              year_range = c(min(years), max(years))
)


# Arrange plots

p <- cowplot::plot_grid(p1, p2, p3,
                        ncol = 1, nrow = 3, rel_heights = c(1, 1, 1.75))

# Save plot

# Plot values
plot_name <- paste0(spp, "-pg-3")

# Save ggplot
ggplot2::ggsave(paste0(fl_path_store,"/", plot_name, fl_type),
                plot = p,
                width = width,
                height = height,
                units = units,
                dpi = dpi
)

# Example 2: With the new function ---------------------------------------------
# Find these functions in new-tidy-lengths-ages.R and new-tidy-comps.R

# Age frequency plot

ages <- tidy_ages(comm_samples,
                  years = years
)

p1 <- plot_commercial_ages(ages,
                           sex = "M",
                           year_range = c(min(years), max(years))
)

p2 <- plot_commercial_ages(ages,
                           sex = "F",
                           year_range = c(min(years), max(years))
)


# Length frequency plot

lengths_sex <- tidy_lengths(comm_samples,
                            bin_size = bin_size,
                            years = years,
                            total = FALSE
)

lengths_total <- tidy_lengths(comm_samples,
                              bin_size = bin_size,
                              years = years,
                              total = TRUE
)

lengths_total <- lengths_total %>%
  dplyr::mutate(sex = "Total")

p3 <- plot_commercial_lengths(lengths_sex, lengths_total,
                              year_range = c(min(years), max(years))
)


# Arrange plots

p <- cowplot::plot_grid(p1, p2, p3,
                        ncol = 1, nrow = 3, rel_heights = c(1, 1, 1.75))

# Save plot

# Plot values
plot_name <- paste0(spp, "-pg-3.1")

# Save ggplot
ggplot2::ggsave(paste0(fl_path_store,"/", plot_name, fl_type),
                plot = p,
                width = width,
                height = height,
                units = units,
                dpi = dpi
)
