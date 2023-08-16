# Adapted from gfplot
plot_commercial_lengths <- function (dat,
                                     totals,
                                     sorted,
                                     xlab = "Length (cm)",
                                     ylab = "Relative length frequency",
                                     #line_col = c("grey40"),
                                     #fill_col = c("grey40"),
                                     # fill_col = c("M" = "#1b9e7750", "F" = "#7570b350"),
                                     # line_col = c("M" = "#1b9e77", "F" = "#7570b3"),
                                     fill_col = c("M" = "grey80", "F" = "#d95f0250"),
                                     line_col = c("M" = "grey40", "F" = "#d95f02"),
                                     alpha = 0.24,
                                     bin_size = 2,
                                     min_total = 20,
                                     show_year = "even",
                                     year_range = NULL) {

  # Define area factor levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  sex_levels <- c("F", "M", "Total")

  # Define breaks --------------------------------------------------------------

  x_breaks <- pretty(c(min(min(dat$length_bin, na.rm = TRUE), min(totals$length_bin, na.rm = TRUE)),
                       max(max(dat$length_bin, na.rm = TRUE), max(totals$length_bin, na.rm = TRUE))), 4L)
  x_breaks <- x_breaks[seq_len(length(x_breaks) - 1L)]

  # Define range ---------------------------------------------------------------

  range_lengths <- diff(c(min(min(dat$length_bin, na.rm = TRUE), min(totals$length_bin, na.rm = TRUE)),
                          max(max(dat$length_bin, na.rm = TRUE), max(totals$length_bin, na.rm = TRUE))))

  years <- seq(year_range[1], year_range[2])

  # dat <- dat %>%
  #   dplyr::filter(year %in% years)
  #
  # dat <- dat %>%
  #   dplyr::arrange(area, year)
  #
  # dat <- dat %>%
  #   dplyr::mutate(area = factor(area, area_levels))
  #
  # dat <- dat %>%
  #   dplyr::arrange(area)
  #
  # # Repeat for totals
  # totals <- totals %>%
  #   dplyr::filter(year %in% years)
  #
  # totals <- totals %>%
  #   dplyr::arrange(area, year)
  #
  # totals <- totals %>%
  #   dplyr::mutate(area = factor(area, area_levels))
  #
  # totals <- totals %>%
  #   dplyr::arrange(area)

  # Define counts --------------------------------------------------------------

  #counts <- subset(dat, dat$sex == "Total")
  counts <- totals %>%
    dplyr::select(survey_abbrev, year, total, area) %>%
    unique()

  counts_sorted <- sorted %>%
    dplyr::select(survey_abbrev, year, total, area) %>%
    unique()

  # Scale each maximum proportion to one ---------------------------------------

  dat <- dat %>%
    dplyr::group_by(year, area) %>%
    dplyr::mutate(new_proportion = proportion / max(proportion, na.rm = FALSE)) %>%
    dplyr::ungroup()

  totals <- totals %>%
    dplyr::group_by(year, area) %>%
    dplyr::mutate(new_proportion = proportion / max(proportion, na.rm = FALSE)) %>%
    dplyr::ungroup()

  dat$sex <- factor(dat$sex, levels = rev(sort(unique(dat$sex)))) # to get F bars shaded on top
  dat <- arrange(dat, year, survey_abbrev, sex)


  totals <- totals %>%
    dplyr::group_by(year, area) %>%
    # add rows for length_bin-1 and length_bin+1
    tidyr::complete(length_bin = c(min(length_bin)-1, length_bin, max(length_bin)+1),
                    fill = list(proportion = 0, new_proportion = 0),
                    explicit = FALSE
    ) %>%
    # fill the missing values with 0
    # tidyr::replace_na(list(proportion = 0, new_proportion = 0)
    # ) %>%
    dplyr::mutate(species_common_name = unique(na.omit(species_common_name)),
                  survey_abbrev = unique(na.omit(survey_abbrev)),
                  sex = unique(na.omit(sex))
    ) %>%
    dplyr::ungroup()

  totals$sex <- factor(totals$sex, levels = sex_levels)

  totals <- totals %>%
    dplyr::arrange(area, year, sex)

  # Remove proportions with scarce observations --------------------------------

  #lengths_all <- lengths_all %>%
  #dplyr::mutate(proportion = ifelse(total >= min_total, proportion, NA))

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(dat, ggplot2::aes(length_bin, new_proportion)
  ) +
    ggplot2::geom_col(
      width = bin_size,
      ggplot2::aes(colour = sex, fill = sex),
      linewidth = 0.2,
      position = ggplot2::position_identity()
    ) +
    gfplot::theme_pbs() +
    ggplot2::scale_fill_manual(values = fill_col, breaks = c("M", "F")) +
    ggplot2::scale_colour_manual(values = line_col, breaks = c("M", "F")) +
    #ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = x_breaks, expand = c(0.01, 0.01)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ylim(0, 1.05) +
    ggplot2::geom_text(
      data = counts,
      x = min(min(dat$length_bin, na.rm = TRUE), min(totals$length_bin, na.rm = TRUE)) + 0.013 * range_lengths,
      y = 0.82,
      mapping = ggplot2::aes(label = total),
      inherit.aes = FALSE,
      colour = "grey50",
      size = 3.0,
      hjust = 0
    ) +
    ggplot2::geom_text(
      data = counts_sorted,
      x = max(max(dat$length_bin, na.rm = TRUE), max(totals$length_bin, na.rm = TRUE)) - 0.013 * range_lengths,
      y = 0.82,
      mapping = ggplot2::aes(label = total),
      inherit.aes = FALSE,
      colour = "royalblue",
      size = 3.0,
      hjust = 1
    ) +
    ggplot2::labs(title = "Length frequencies") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 17, vjust = -2.5),
      axis.title = ggplot2::element_text(size = 14),
      axis.text.x.bottom = ggplot2::element_text(size = 11),
      axis.text.y.left = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 12, vjust = -0.8),
      strip.text.y = ggplot2::element_text(size = 10.5),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey93"),
      panel.spacing = grid::unit(-0.06, "lines"),
      plot.margin = grid::unit(c(-0.35, 0, 0, 0.5), "cm"),
      legend.position = "none"
    )

p1 <- p1 +
  ggplot2::geom_step(data = totals,
                     ggplot2::aes(x = length_bin, y = new_proportion),
                     col = "grey20",
                     linewidth = 0.2,
                     direction = "mid"
                     )

# for (i in 1:nrow(counts_sorted)) {
#
#   if (!is.na(counts_sorted$total[i])) {
#     p1 <- p1 +
#       ggplot2::geom_text(
#         data = counts_sorted,
#         x = min(min(dat$length_bin, na.rm = TRUE), min(totals$length_bin, na.rm = TRUE)) + 0.013 * range_lengths,
#         y = 0.20,
#         mapping = ggplot2::aes(label = counts_sorted$total[i]),
#         inherit.aes = FALSE,
#         colour = "grey50",
#         size = 3.0,
#         hjust = 0
#       )
#     }
#   }


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
