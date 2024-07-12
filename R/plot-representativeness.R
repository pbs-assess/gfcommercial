# Adapted from gfplot
plot_representativeness <- function (data,
                                     xlab = "Week of the year",
                                     ylab = "Cumulative Proportion",
                                     line_col = c("grey40"),
                                     fill_col = c("grey40"),
                                     min_total = 20,
                                     show_year = "even") {

  # Define counts --------------------------------------------------------------

  # Define counts --------------------------------------------------------------

  counts <- data %>%
    dplyr::select(species_common_name,
                  area,
                  year,
                  n_catch,
                  n_samples) %>%
    unique()

  # Scale units

  counts <- counts %>%
    dplyr::mutate(catch_text = catch_rounding(n_catch),
                  samp_text = round_nice(n_samples))

  # Turn zeros into NA

  data$catch_prop[data$catch_prop== 0] <- NA
  data$samples_prop[data$samples_prop == 0] <- NA
  data$spatial_prop[data$spatial_prop == 0] <- NA

  # Adjust so each line starts at zero (for both catch and samples)

  data_c <- data %>%
    dplyr::select(species_common_name, area, year, week, catch_prop) %>%
    tidyr::drop_na(catch_prop)

  data_c <- data_c %>%
    dplyr::group_by(species_common_name, year, area) %>%
    tidyr::complete(week = c(min(week)-1, week),
                    fill = list(catch_prop = 0),
                    explicit = FALSE) %>%
    dplyr::ungroup()

  data_s <- data %>%
    dplyr::select(species_common_name, area, year, week, samples_prop) %>%
    tidyr::drop_na(samples_prop)

  data_s <- data_s %>%
    dplyr::group_by(species_common_name, year, area) %>%
    tidyr::complete(week = c(min(week)-1, week),
                    fill = list(samples_prop = 0),
                    explicit = FALSE) %>%
    dplyr::ungroup()

  data_sp <- data %>%
    dplyr::select(species_common_name, area, year, week, spatial_prop) %>%
    tidyr::drop_na(spatial_prop)

  data_sp <- data_sp %>%
    dplyr::group_by(species_common_name, year, area) %>%
    tidyr::complete(week = c(min(week)-1, week),
                    fill = list(spatial_prop = 0),
                    explicit = FALSE) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::select(-catch_prop, -samples_prop, -spatial_prop)

  data <- dplyr::left_join(data, data_c, by = c("species_common_name", "area", "year", "week"))
  data <- dplyr::left_join(data, data_s, by = c("species_common_name", "area", "year", "week"))
  data <- dplyr::left_join(data, data_sp, by = c("species_common_name", "area", "year", "week"))

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = week)
  ) +
    ggplot2::geom_area(
      ggplot2::aes(y = samples_prop), # Samples second so the thinner, lighter line is on top
      colour = "#f56d05",
      fill = "#f56d05",
      linewidth = 0.6,
      linetype = 2,
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = spatial_prop),
      colour = "#f56d05",
      linewidth = 0.6,
      alpha = 1
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = catch_prop),
      colour = "black",
      linewidth = 0.25,
    ) +
    gfplot::theme_pbs() +
    ggplot2::coord_cartesian(expand = FALSE) +
    #ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    # ggplot2::ylim(-0.22, 1.10) +
    # ggplot2::xlim(1, 53) +
    ggplot2::coord_cartesian(xlim = c(1,53),
                             ylim = c(-0.1, 1.10),
                             expand = FALSE) +
    ggplot2::geom_text(
      data = counts,
      x = 51.4,
      y = 0.15,
      mapping = ggplot2::aes(label = catch_text),
      inherit.aes = FALSE,
      colour = "black",
      size = 3.1,
      hjust = 1
    ) +
    ggplot2::geom_text(
      data = counts,
      x = 2.2,
      y = 0.89,
      mapping = ggplot2::aes(label = samp_text),
      inherit.aes = FALSE,
      colour = "#f56d05",
      size = 3.1,
      hjust = 0
    ) +
    ggplot2::labs(title = "Representativeness") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 17, vjust = -2),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      axis.text.x.bottom = ggplot2::element_text(size = 11),
      axis.text.y.left = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 14, vjust = -0.7),
      strip.text.y = ggplot2::element_text(size = 14),
      axis.ticks.y = ggplot2::element_blank(),
      #panel.grid.major.x = ggplot2::element_line(colour = "grey93"),
      panel.spacing = grid::unit(-0.15, "lines"),
      plot.margin = grid::unit(c(0, 0, 0, 0.27), "cm")
    )

  # Facet grid -----------------------------------------------------------------

  if (show_year == "even") {
    p1 <- p1 +
      ggplot2::facet_grid(
        forcats::fct_rev(as.factor(year)) ~ area,
        labeller = ggplot2::labeller(.rows = gfplot:::is_even),
        drop = FALSE
      )
  } else if (show_year == "odd") {
    p1 <- p1 +
      ggplot2::facet_grid(
        forcats::fct_rev(as.factor(year)) ~ area,
        labeller = ggplot2::labeller(.rows = gfplot:::is_odd),
        drop = FALSE
      )
  } else {
    p1 <- p1 +
      facet_grid(
        forcats::fct_rev(as.factor(year)) ~ area,
        labeller = ggplot2::labeller(.rows = gfplot:::is_all),
        drop = FALSE
      )
  }

  # Return plot ----------------------------------------------------------------

  return(p1)
}
