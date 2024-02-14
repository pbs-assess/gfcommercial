# Plot samples by gear type
# Modified from GF Synopsis

mround <- function(x, base) {
  base * round(x / base)
}


plot_samples <- function(dat,
                         blank_plot = FALSE,
                         years = NULL,
                         french = FALSE,
                         ...) {

  # Define years ---------------------------------------------------------------

  if (is.null(years)) {
    years <- seq(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE), 1L)
  }

  year_range = c(min(years), max(years))

  unreliable <- c(1995, 2005.5)

  # Plot samples ---------------------------------------------------------------

  gears <- c("Bottom trawl",
             "Midwater trawl",
             "Hook and line",
             "Trap",
             "Unknown/trawl",
             "Discarded"
             )

  pal <- c(RColorBrewer::brewer.pal(n = length(gears) - 2, "Paired"), "grey60", "grey30")[c(2, 1, 4, 3, 5, 6)]

  names(pal) <- gears

  dat$gear <- factor(dat$gear, levels = gears)

  if (!blank_plot) {
    dat <- left_join(
      expand.grid(year = seq(min(year_range), max(year_range)),
                  area = levels(as.factor(dat$area)), gear = levels(dat$gear)
                  ),
      dat,
      by = c("year", "area", "gear")
      )

    dat$value[is.na(dat$value)] <- 0
  }

  yrs <- year_range

  g <- ggplot2::ggplot(data = dat)

  g <- g +
    ggplot2::geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey98") +
    ggplot2::geom_vline(xintercept = seq(mround(yrs[1], 5), yrs[2], 5), col = "grey95")

  scale_val <- 1

  ylab_gg <- "Count"

  stacked_data <- group_by(dat, area, year) %>%
    summarise(samples = sum(value / scale_val, na.rm = FALSE))

  if (!is.na(unreliable[[1]])) {
    for (i in seq_along(unreliable)) {
      g <- g + ggplot2::geom_rect(
        data = data.frame(x = NA), # fake
        xmin = year_range[1] - 1,
        xmax = unreliable[[i]], ymin = 0,
        ymax = max(stacked_data$samples, na.rm = TRUE) * 1.07,
        fill = "#00000010", inherit.aes = FALSE
      )
    }
  }

  if (!blank_plot) {
    g <- g + ggplot2::geom_col(data = dat,
                               ggplot2::aes(year, value/scale_val,
                                            colour = gear,
                                            fill = gear)
                               )
      #ggplot2::ylim(0, max(stacked_data$samples, na.rm = TRUE) * 1.1)
  }

  if (blank_plot) {
    g <- g + ggplot2::ylim(0, 1)
  }

  g <- g +
    gfplot::theme_pbs() +
    ggplot2::scale_fill_manual(values = pal,
                               drop = FALSE,
                               breaks = gears,
                               labels = gears) +
    ggplot2::scale_colour_manual(values = pal,
                                 drop = FALSE,
                                 breaks = gears,
                                 labels = gears) +
    ggplot2::coord_cartesian(xlim = year_range + c(-0.5, 0.5), expand = FALSE) +
    ggplot2::xlab("") +
    ggplot2::ylab(ylab_gg) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = "", colour = "") +
    ggplot2::theme(legend.background = ggplot2::element_rect(fill = "#FFFFFF99"))


  if (!all(dat$area == "Total")) {
    g <- g + ggplot2::facet_wrap(~area, ncol = 1)
  }

  g


  g <- g +
    #ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines")) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, year_range[2], 5))+
    ggplot2::scale_y_continuous(limits = c(0, max(stacked_data$samples, na.rm = TRUE) * 1.05), position = "right")

  if (blank_plot)
    suppressMessages(g <- g + ggplot2::ylim(0, 1))

  gdat <- ggplot2::ggplot_build(g)$data

  max_val <- if (!blank_plot) max(gdat[[5]]$ymax) else 1

  labs <- unique(select(dat, area))

  .mult <- 0.91 # vertical area label

  g <- g + ggplot2::geom_text(data = labs,
                     x = year_range[1] - 0.2,
                     y = max_val * .mult,
                     ggplot2::aes(label = area),
                     inherit.aes = FALSE,
                     colour = "grey30",
                     size = 3.2,
                     hjust = 0
                     )

  g <- g + ggplot2::theme(plot.title = ggplot2::element_text(size = 17),
                          legend.justification = c(0, 1),
                          legend.position = c(0, 0.2),
                          legend.text = ggplot2::element_text(size = 9),
                          legend.background = ggplot2::element_blank(),
                          legend.direction = "horizontal",
                          axis.title.y = ggplot2::element_text(size = 14),
                          axis.text.x = ggplot2::element_text(size = 12),
                          axis.text.y.right = ggplot2::element_text(size = 10),
                          plot.margin = ggplot2::unit(c(0.2, 0.2, -0.2, 0.2), "cm"),
                          axis.text.y.left = ggplot2::element_blank()
                          ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ggtitle("Commercial specimens")

  g

}
