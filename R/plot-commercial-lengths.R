# Adapted from gfplot and gfbiosampling

plot_commercial_lengths <- function (data,
                                     xlab = "Length (cm)",
                                     ylab = "Relative length frequency",
                                     line_col = c("grey40"),
                                     fill_col = c("grey40"),
                                     alpha = 0.5,
                                     bin_size = 2,
                                     min_total = 20,
                                     show_year = "even") {
  
  # Define breaks --------------------------------------------------------------
  
  x_breaks <- pretty(data$length_bin, 4L)
  x_breaks <- x_breaks[seq_len(length(x_breaks) - 1L)]
  
  # Define range ---------------------------------------------------------------
  
  range_lengths <- diff(range(data$length_bin, na.rm = TRUE))
  
  # Define counts --------------------------------------------------------------
  
  counts <- data %>%
    dplyr::select(
      survey_abbrev,
      year,
      total,
      area
    ) %>%
    unique()
  
  # Scale each maximum proportion to one ---------------------------------------
  
  data <- data %>%
    dplyr::group_by(year, survey_abbrev, area) %>%
    dplyr::mutate(proportion = proportion / max(proportion)) %>%
    dplyr::ungroup()
  
  # Remove proportions with scarce observations --------------------------------
  
  data <- data %>%
    dplyr::mutate(proportion = ifelse(total >= min_total, proportion, NA))
  
  # Assemble plot --------------------------------------------------------------
  
  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(length_bin, proportion)
  ) +
    ggplot2::geom_col(
      width = bin_size,
      color = line_col,
      fill = scales::alpha(fill_col, alpha),
      size = 0.3,
      position = ggplot2::position_identity()
    ) +
    gfplot::theme_pbs() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ylim(-0.04, 1.07) +
    ggplot2::geom_text(
      data = counts,
      x = min(data$length_bin, na.rm = TRUE) + 0.02 * range_lengths,
      y = 0.85,
      mapping = ggplot2::aes(label = total),
      inherit.aes = FALSE,
      colour = "grey50",
      size = 2.25,
      hjust = 0
    ) +
    ggplot2::labs(title = "Length frequencies") +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(size = 5.5),
      axis.text.y.left = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(size = 8.0),
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
