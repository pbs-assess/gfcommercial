# Adapted from gfplot
plot_commercial_lengths <- function (dat,
                                     xlab = "Length (cm)",
                                     ylab = "Relative length frequency",
                                     #line_col = c("grey40"),
                                     #fill_col = c("grey40"),
                                     #fill_col = c("M" = "grey80", "F" = "#FF000010"),
                                     #line_col = c("M" = "grey40", "F" = "red"),
                                     alpha = 0.24,
                                     bin_size = 2,
                                     min_total = 20,
                                     show_year = "even",
                                     year_range = NULL) {


# Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

# Define colours for female lengths
  survey_cols <- c(RColorBrewer::brewer.pal(8L, "Set2"),
                  "#303030")
  survey_col_names <- area_levels

  survey_cols <- stats::setNames(survey_cols, survey_col_names)

  #survey_col_names <- names(survey_cols)
  col <- stats::setNames(survey_cols, paste("F", survey_col_names))
  col <- c(col, stats::setNames(rep("#888888", length(col)),
                                paste("M", survey_col_names)
  ))
  fill_col <- paste0(substr(col, 1L, 8L), as.character(alpha * 100))
  names(fill_col) <- c(survey_col_names, survey_col_names)
  line_col <- col
  names(fill_col) <- names(line_col)
  dat$sex <- paste(dat$sex, dat$survey_abbrev)


  # Define breaks --------------------------------------------------------------

  x_breaks <- pretty(dat$length_bin, 4L)
  x_breaks <- x_breaks[seq_len(length(x_breaks) - 1L)]

  # Define range ---------------------------------------------------------------

  range_lengths <- diff(range(dat$length_bin, na.rm = TRUE))

  years <- seq(year_range[1], year_range[2])

  dat <- dat %>%
    dplyr::filter(year %in% years)

   dat <- dat %>%
     dplyr::arrange(area, year)

   dat <- dat %>%
     dplyr::mutate(area = factor(area, area_levels))

   dat <- dat %>%
     dplyr::arrange(area)

  # Define counts --------------------------------------------------------------

  counts <- dat %>%
    dplyr::select(survey_abbrev, year, total, area) %>%
    unique()

  # Scale each maximum proportion to one ---------------------------------------

  dat <- dat %>%
    dplyr::group_by(year, area) %>%
    dplyr::mutate(new_proportion = proportion / max(proportion, na.rm = TRUE)) %>%
    dplyr::ungroup()

   dat$sex <- factor(dat$sex, levels = rev(sort(unique(dat$sex)))) # to get F bars shaded on top
   dat <- arrange(dat, year, survey_abbrev, sex)

  # Remove proportions with scarce observations --------------------------------

  #lengths_all <- lengths_all %>%
    #dplyr::mutate(proportion = ifelse(total >= min_total, proportion, NA))

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(dat, ggplot2::aes(length_bin, new_proportion)
    ) +
    ggplot2::geom_col(
      width = bin_size,
      ggplot2::aes(colour = sex, fill = sex),
      #color = line_col,
      #fill = scales::alpha(fill_col, alpha),
      size = 0.3,
      position = ggplot2::position_identity()
    ) +
    gfplot::theme_pbs() +
    ggplot2::scale_fill_manual(values = fill_col, breaks = c("M", "F")) +
    ggplot2::scale_colour_manual(values = line_col, breaks = c("M", "F")) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ylim(-0.06, 1.15) +
    ggplot2::geom_text(
      data = counts,
      x = min(dat$length_bin, na.rm = TRUE) + 0.013 * range_lengths,
      y = 0.82,
      mapping = ggplot2::aes(label = total),
      inherit.aes = FALSE,
      colour = "grey50",
      size = 2.0,
      hjust = 0
    ) +
    ggplot2::labs(title = "Length frequencies") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(vjust = -4),
      axis.text.x.bottom = ggplot2::element_text(size = 5.5),
      axis.text.y.left = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(vjust = -1.5),
      strip.text.y = ggplot2::element_text(size = 5.5),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey93"),
      panel.spacing = grid::unit(-0.06, "lines"),
      plot.margin = grid::unit(c(-3.5, 0, 0, 1), "mm")
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
