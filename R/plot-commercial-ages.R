# Adapted from gfplot
plot_commercial_ages <- function (data,
                                  max_size = 5,
                                  sex_gap = 0.2,
                                  year_increment = 2,
                                  ylab = "Age (years)",
                                  year_range = NULL,
                                  line_col = c("M" = "#1b9e77", "F" = "#7570b3"),
                                  alpha = 0.2,
                                  grid_col = "grey95",
                                  diagonal_lines = seq(-2100, -1850, 10),
                                  count_label_size = 1.25) {

  # Set maximum age ------------------------------------------------------------

  if (nrow(data) > 0) {
    age_max <- max(data$age, na.rm = TRUE)
  } else {
    age_max <- 1
  }

  # Jitter sexes ---------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(
      year_jitter = ifelse(
        sex == "M",
        year - sex_gap / 2,
        year + sex_gap / 2
      )
    )

  # Define counts --------------------------------------------------------------

  counts <- data %>%
    dplyr::select(
      total,
      year,
      area
    ) %>%
    unique()

  # Define age range -----------------------------------------------------------

  age_range <- diff(range(data$age, na.rm = TRUE))

  # Define year range ----------------------------------------------------------

  if (is.null(year_range)) {
    year_range <- c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE))
  }

  # Accommodate empty plot -----------------------------------------------------

  if (sum(!is.na(data$age)) == 0) {
    data$age <- 0
    age_range <- 1
  }

  # Define sex as factor -------------------------------------------------------

  data <- data %>%
    dplyr::mutate(sex = factor(sex, levels = c("M", "F"))) %>% # F on top
    dplyr::arrange(area, year, sex)

  # Augment by missing areas ---------------------------------------------------

  # Get area levels
  area_levels <- levels(data$area)
  # Assemble placeholder rows
  placeholder <- data[seq_along(area_levels), ] %>%
    dplyr::mutate(age = 0) %>%
    dplyr::mutate(proportion = 0) %>%
    dplyr::mutate(total = NA_real_) %>%
    dplyr::mutate(area = factor(area_levels, levels = area_levels))
  # Augment data
  data <- data %>%
    dplyr::bind_rows(placeholder)

  # Plot ages ------------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(year_jitter, age)
  ) +
    ggplot2::facet_grid(cols = ggplot2::vars(area)) +
    ggplot2::scale_x_continuous(
      breaks = seq(
        gfplot:::round_down_even(min(year_range)),
        max(year_range),
        year_increment
      )
    ) +
    ggplot2::geom_vline(
      xintercept = seq(year_range[1], year_range[2], 1),
      col = grid_col,
      lwd = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = seq(0, age_max, 10),
      col = grid_col,
      lwd = 0.4
    ) +
    ggplot2::coord_cartesian(
      xlim = year_range + c(-0.8 - sex_gap / 2, 0.8 + sex_gap / 2),
      ylim = c(0, age_max + 0.02 * age_range),
      expand = FALSE
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab(ylab) +
    ggplot2::labs(
      title = "Age frequencies",
      colour = "Sex",
      fill = "Sex"
    ) +
    gfplot::theme_pbs() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(vjust = -4),
      axis.text.x = ggplot2::element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
      strip.text.x = ggplot2::element_text(vjust = -2),
      legend.position = "none",
      panel.spacing = grid::unit(-0.1, "lines"),
      plot.margin = grid::unit(c(-3.5, 1, -4, 1), "mm")
    )

  # Conditionally include ablines and text -------------------------------------

  if (sum(data$age > 0, na.rm = TRUE) > 0) {
    p1 <- p1 +
      ggplot2::geom_abline(
        intercept = diagonal_lines,
        slope = 1,
        colour = grid_col
      ) +
      ggplot2::scale_fill_manual(
        values = scales::alpha(line_col, alpha),
        breaks = c("M", "F")
      ) +
      ggplot2::scale_colour_manual(
        values = line_col,
        breaks = c("M", "F")
      ) +
      ggplot2::scale_size_area(max_size = max_size) +
      ggplot2::geom_text(
        data = counts,
        y = age_max + 0.005 * age_range,
        mapping = ggplot2::aes(x = year, label = total),
        inherit.aes = FALSE,
        colour = "grey50",
        size = count_label_size,
        hjust = 1,
        angle = 90
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          size = proportion,
          group = sex,
          fill = sex,
          colour = sex
        ),
        pch = 21
      )
  }

  # Return plot ----------------------------------------------------------------

  return(p1)
}
