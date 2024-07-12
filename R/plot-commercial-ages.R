# Adapted from gfplot
plot_commercial_ages <- function (data,
                                  data_sorted,
                                  max_size = 5,
                                  sex_gap = 0.2,
                                  sex = c("M", "F"),
                                  year_increment = 4,
                                  ylab = "Age (years)",
                                  year_range = NULL,
                                  line_col = c("M" = "#33A02C", "F" = "#B23AEE", "sorted" = "grey60"),
                                  fill_col = c("M" = "#33A02C30", "F" = "#B23AEE30", "sorted" = "#FFFFFF10"),
                                  alpha = 0.2,
                                  grid_col = "grey95",
                                  diagonal_lines = seq(-2100, -1850, 10),
                                  count_label_size = 1) {

  # Set maximum age ------------------------------------------------------------

  if (sum(data$age, na.rm = TRUE) > 0) {
    age_max <- max(data$age, na.rm = TRUE)
  } else if (sum(data_sorted$age, na.rm = TRUE) > 0) {
    age_max <- max(data_sorted$age, na.rm = TRUE)
  } else {
    age_max <- 1
  }

  # Separate sexes -------------------------------------------------------------

  if (sex == "M") {
    data <- subset(data, sex == "M")
  } else if (sex =="F") {
    data <- subset(data, sex == "F")
  }

  if (sex == "M") {
    data_sorted <- subset(data_sorted, sex == "M")
    data_sorted <- data_sorted %>%
      dplyr::mutate(sex = "sorted")
  } else if (sex =="F") {
    data_sorted <- subset(data_sorted, sex == "F")
    data_sorted <- data_sorted %>%
      dplyr::mutate(sex = "sorted")
  }

  data <- dplyr::bind_rows(data, data_sorted)

  sex_levels <- c("F", "M", "sorted")

  data$sex <- factor(data$sex, levels = sex_levels)


  # Jitter sexes ---------------------------------------------------------------

  if (sex == "M") {
    data <- data %>%
      dplyr::mutate(
        year_jitter = ifelse(
          sex == "M",
          year + sex_gap / 2,
          year - sex_gap / 2
        )
      )
  } else if (sex == "F") {
    data <- data %>%
      dplyr::mutate(
        year_jitter = ifelse(
          sex == "F",
          year + sex_gap / 2,
          year - sex_gap / 2
        )
      )
  }

  data <- data %>%
    dplyr::arrange(area, year_jitter, sex)


  # Define year range ----------------------------------------------------------

  if (is.null(year_range)) {
    year_range <- c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE))
  }

  # Filter for target years ----------------------------------------------------

  years <- seq(min(year_range), max(year_range))

  data <- data %>%
    dplyr::filter(year %in% years)

  # Define age range and accommodate empty plot --------------------------------

  if (sum(!is.na(data$age)) == 0) {
    data$age <- 0
    age_range <- 1
  } else {
    age_range <- diff(range(data$age, na.rm = TRUE))
  }

  # Remove outlier data for ARF

  if (unique(data$species_common_name == "arrowtooth flounder")) {

    data <- data %>%
      dplyr::mutate(age = dplyr::coalesce(age, 0)) %>%
      dplyr::filter(age < 30, na.rm = TRUE)

    age_max <- max(data$age, na.rm = TRUE)

    data["age"][data["age"] == 0] <- NA

    age_range <- diff(range(data$age, na.rm = TRUE))


  }

  ages_all <- data


  # Plot ages ------------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    ages_all,
    ggplot2::aes(year_jitter, age)
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(as.factor(area))) +
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
    gfplot::theme_pbs() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 17, vjust = -3),
      axis.title.y = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 11, angle = 90, hjust = 1, vjust = 0.5),
      strip.text.x = ggplot2::element_text(size = 12, vjust = -0.8),
      legend.position = "none",
      panel.spacing = grid::unit(-0.1, "lines")
    )

  if (sex == "M") {
    p1 <- p1 +
      ggplot2::labs(
      title = "Age frequencies - Male",
      colour = "Sex",
      fill = "Sex"
    ) +
      ggplot2::theme(
        plot.margin = grid::unit(c(-0.5, 0.6, -0.3, 0.1), "cm")
      )
  } else if (sex == "F") {
    p1 <- p1 +
      ggplot2::labs(
      title = "Age frequencies - Female",
      colour = "Sex",
      fill = "Sex"
    ) +
      ggplot2::theme(
        plot.margin = grid::unit(c(-0.35, 0.6, -0.3, 0.1), "cm")
      )
  }

  # Conditionally include ablines and text -------------------------------------

  if (sum(ages_all$age > 0, na.rm = TRUE) > 0) {
    p1 <- p1 +
      ggplot2::geom_abline(
        intercept = diagonal_lines,
        slope = 1,
        colour = grid_col
      ) +
      ggplot2::scale_fill_manual(values = fill_col,
                                 breaks = c("M", "F", "sorted")
      ) +
      ggplot2::scale_colour_manual(values = line_col,
                                   breaks = c("M", "F", "sorted")
      ) +
      ggplot2::scale_size_area(max_size = max_size) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          size = proportion,
          group = sex,
          fill = sex,
          colour = sex
        ),
        pch = 21,
        na.rm = TRUE
      ) +
      ggplot2::scale_size_continuous(range = c(0.01, 4)
      )
  }

  # Return plot ----------------------------------------------------------------

  return(p1)
}
