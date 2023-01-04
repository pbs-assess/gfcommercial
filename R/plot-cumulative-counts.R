# Adapted from gfplot
plot_cumulative_counts <- function (data,
                                    xlab = "Week of the year",
                                    ylab = "Proportion",
                                    line_col = c("grey40"),
                                    fill_col = c("grey40"),
                                    alpha = 0.5,
                                    bin_size = 2,
                                    min_total = 20,
                                    show_year = "even") {
  
  # Define breaks --------------------------------------------------------------
  
  #x_breaks <- pretty(data$length_bin, 4L)
  #x_breaks <- x_breaks[seq_len(length(x_breaks) - 1L)]
  
  # Define range ---------------------------------------------------------------
  
  #range_lengths <- diff(range(data$length_bin, na.rm = TRUE))
  
  # Define counts --------------------------------------------------------------
  
  counts <- data %>%
    dplyr::select(
      species_common_name,
      area,
      year,
      n_catch,
      n_samples
    ) %>%
    unique()
  
  # Scale each maximum proportion to one ---------------------------------------
  
  #data <- data %>%
  #  dplyr::group_by(year, survey_abbrev, area) %>%
  #  dplyr::mutate(proportion = proportion / max(proportion)) %>%
  #  dplyr::ungroup()
  
  # Remove proportions with scarce observations --------------------------------
  
  #data <- data %>%
  #  dplyr::mutate(proportion = ifelse(total >= min_total, proportion, NA))
  
  # Assemble plot --------------------------------------------------------------
  
  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = week)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(y = catch_prop),
      colour = "black",
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = samples_prop), # Samples second so the thinner, lighter line is on top
      colour = "grey50",
      linewidth = 0.25
    ) +
    gfplot::theme_pbs() +
    ggplot2::coord_cartesian(expand = FALSE) +
    #ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ylim(-0.15, 1.15) +
    ggplot2::xlim(1, 53) +
    ggplot2::geom_text(
      data = counts,
      x = 2,
      y = 0.95,
      mapping = ggplot2::aes(label = n_catch),
      inherit.aes = FALSE,
      colour = "black",
      size = 1.5,
      hjust = 0
    ) +
    ggplot2::geom_text(
      data = counts,
      x = 51.5,
      y = 0.18,
      mapping = ggplot2::aes(label = n_samples),
      inherit.aes = FALSE,
      colour = "grey50",
      size = 1.5,
      hjust = 1
    ) +
    ggplot2::labs(title = "Representativeness") +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(size = 5.5),
      axis.text.y.left = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(size = 7.5),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey93"),
      panel.spacing = grid::unit(-0.1, "lines")
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
