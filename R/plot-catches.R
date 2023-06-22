# Modified from GF synopsis

plot_catches <- function(dat, blank_plot = FALSE, years = NULL,
                         french = FALSE, ...) {

  # Define years ---------------------------------------------------------------

  if (is.null(years)) {
    years <- seq(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE), 1L)
  }

  year_range = c(min(years), max(years))

  # Plot catches ---------------------------------------------------------------

  g <- gfplot::plot_catch(dat, xlim = year_range, french = french, ...) +
    #ggplot2::theme(panel.spacing = ggplot2::unit(-0.1, "lines")) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, year_range[2], 5))

  if (blank_plot)
    suppressMessages(g <- g + ylim(0, 1))

  gdat <- ggplot2::ggplot_build(g)$data
  # data_element <- which(unlist(lapply(lapply(gdat, names),
  #   function(x) "ymax" %in% x)))[[1]]
  # gdat <- gdat[[data_element]]
  # max_val <- max(gdat$ymax)
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
                          axis.text.y.left = ggplot2::element_text(size = 10),
                          plot.margin = ggplot2::unit(c(0.2, 0.2, 0.5, 0.2), "cm")
                          ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::ggtitle("Commercial catch")

  if (french) {
    g <- g + ggplot2::theme(legend.spacing.x = ggplot2::unit(0.5, "mm"))
    g <- g +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) # e.g. 1 000
  }

  g

}
