
# Adapted from gfplot::plot_sample_avail
plot_commercial_counts <- function (data,
                                    years = NULL,
                                    sorted = TRUE,
                                    text_colour = "white",
                                    na_colour = "white") {

  # Define years ---------------------------------------------------------------

  if (is.null(years)) {
    years <- seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1L)
  }

  # Subset for target years ----------------------------------------------------

  data <- data %>%
    dplyr::filter(year %in% years)

  # Define title ---------------------------------------------------------------

  if (sorted) {
    title <- "Sorted commercial specimen counts"
  } else if (!sorted) {
    title <- "Unsorted commercial specimen counts"
  }

  # Augment data ---------------------------------------------------------------

  data <- data %>%
    dplyr::filter(year %in% years) %>%
    dplyr::mutate(
      n_plot = sqrt(n),
      n_text = gfplot:::round_nice(n),
      type = gfplot:::firstup(as.character(gsub("_", " ", type))),
      type = gsub("Ageing structure", "Age structures", type),
      type = paste("#", type, sep = " ")
    )

  # Expand all combinations ----------------------------------------------------

  all <- tidyr::expand_grid(
    type = unique(data$type),
    area = factor(levels(data$area), levels = levels(data$area)),
    year = years
  )

  # Join data and combinations -------------------------------------------------

  data <- data %>%
    dplyr::right_join(
      all,
      by = c("type", "area", "year")
    ) %>%
    dplyr::mutate(
      n_plot = ifelse(
        is.na(n_plot), NA,
        ifelse(
          n_plot == 0, NA,
          n_plot
        )
      )
    ) %>%
    dplyr::mutate(
      type = factor(
        type,
        levels = rev(
          c(
            "# Length",
            "# Weight",
            "# Maturity",
            "# Age",
            "# Age structures",
            "# Spatial",
            "# Fishing events"
          )
        )
      )
    )

  data <- data %>%
    dplyr::mutate(n = as.numeric(n),
                  n = ifelse(is.na(n), 0, n))

  data <- data %>%
    dplyr::group_by(area, type) %>%
    dplyr::mutate(type_scl = (n /(max(n)))) %>%
    dplyr::mutate(type_scl = ifelse(is.na(type_scl), NA,ifelse(type_scl == 0, NA,type_scl)))

  # Plot specimen counts -------------------------------------------------------
  if (sum(data$n) == 0) {

    p1 <- ggplot2::ggplot(
      data,
      mapping = ggplot2::aes(year, type, fill = type_scl)
    ) +
      ggplot2::geom_tile(colour = "grey90") +
      ggplot2::facet_grid(rows = ggplot2::vars(area)) +
      ggplot2::scale_x_continuous(
        breaks = seq(gfplot:::round_down_even(min(years)), max(years), 2)
      ) +
      ggplot2::scale_y_discrete(position = "left") +
      viridis::scale_fill_viridis(
        option = "D",
        end = 0.82,
        na.value = na_colour,
        discrete = TRUE
      ) +
      ggplot2::coord_cartesian(
        xlim = c(min(years), max(years)) + c(-0.5, 0.5),
        expand = FALSE
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::geom_text(
        ggplot2::aes(x = year, label = n_text),
        colour = text_colour,
        size = 3.2,
        alpha = 1,
        na.rm = TRUE,
        vjust = 0.4
      ) +
      ggplot2::ggtitle(title) +
      gfplot::theme_pbs() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_text(angle = 0, size = 10),
        legend.position = "none",
        panel.spacing = ggplot2::unit(0.7, "lines"),
        plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
      )

  } else {

    p1 <- ggplot2::ggplot(
      data,
      mapping = ggplot2::aes(year, type, fill = type_scl)
    ) +
      ggplot2::geom_tile(colour = "grey90") +
      ggplot2::facet_grid(rows = ggplot2::vars(area)) +
      ggplot2::scale_x_continuous(
        breaks = seq(gfplot:::round_down_even(min(years)), max(years), 2)
      ) +
      viridis::scale_fill_viridis(
        option = "D",
        end = 0.82,
        na.value = na_colour
      ) +
      ggplot2::coord_cartesian(
        xlim = c(min(years), max(years)) + c(-0.5, 0.5),
        expand = FALSE
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::geom_text(
        ggplot2::aes(x = year, label = n_text),
        colour = text_colour,
        size = 3.2,
        alpha = 0.8,
        fontface = "bold",
        na.rm = TRUE,
        vjust = 0.5
      ) +
      ggplot2::ggtitle(title) +
      gfplot::theme_pbs()

      if (sorted == TRUE) {
      p1 <- p1 +
        ggplot2::scale_y_discrete(position = "left") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 17),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 13),
          strip.text.y = ggplot2::element_text(angle = 0, size = 17),
          legend.position = "none",
          panel.spacing = ggplot2::unit(0.7, "lines"),
          plot.margin = ggplot2::unit(c(0, -0.1, -0.5, 0), "cm")
        )
      } else if (sorted == FALSE) {
      p1 <- p1 + ggplot2::theme(
        plot.title = ggplot2::element_text(size = 17),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 13),
        axis.text.y.left = ggplot2::element_text(size = 13),
        strip.text.y = ggplot2::element_blank(),
        legend.position = "none",
        panel.spacing = ggplot2::unit(0.7, "lines"),
        plot.margin = ggplot2::unit(c(0, 0.25, -0.5, -0.3), "cm")
      )
      }

  }


  # Return plot ----------------------------------------------------------------

  return(p1)
}
