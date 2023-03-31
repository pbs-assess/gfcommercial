# Adapted from gfplot
plot_commercial_ages <- function (data,
                                  max_size = 5,
                                  sex_gap = 0.2,
                                  sex = c("M", "F"),
                                  year_increment = 4,
                                  ylab = "Age (years)",
                                  year_range = NULL,
                                  line_col = c("M" = "#1b9e77", "F" = "#7570b3"),
                                  alpha = 0.2,
                                  grid_col = "grey95",
                                  diagonal_lines = seq(-2100, -1850, 10),
                                  count_label_size = 1) {

  # Set maximum age ------------------------------------------------------------

  if (nrow(data) > 0) {
    age_max <- max(data$age, na.rm = TRUE)
  } else {
    age_max <- 1
  }

  # Separate sexes -------------------------------------------------------------

  if (sex == "M") {
    data <- subset(data, sex == "M")
  } else if (sex =="F") {
    data <- subset(data, sex == "F")
  }

  # Jitter sexes ---------------------------------------------------------------

#  data <- data %>%
#    dplyr::mutate(
#      year_jitter = ifelse(
#        sex == "M",
#        year - sex_gap / 2,
#        year + sex_gap / 2
#      )
#    )

  # Define counts --------------------------------------------------------------

#  counts <- ages_all %>%
#    dplyr::select(
#      total,
#      year,
#      area
#    ) %>%
#    unique()

  # Define age range -----------------------------------------------------------

  age_range <- diff(range(data$age, na.rm = TRUE))

  # Define year range ----------------------------------------------------------

  if (is.null(year_range)) {
    year_range <- c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE))
  }

  # Filter for target years ----------------------------------------------------

  years <- seq(min(year_range), max(year_range))

  data <- data %>%
    dplyr::filter(year %in% years)

  # Accommodate empty plot -----------------------------------------------------

  if (sum(!is.na(data$age)) == 0) {
    data$age <- 0
    age_range <- 1
  }

  # Define sex as factor -------------------------------------------------------

#  data <- data %>%
 #   dplyr::mutate(sex = factor(sex, levels = c("M", "F"))) %>% # F on top
  #  dplyr::arrange(area, year, sex)

  # Augment by missing areas ---------------------------------------------------

  # Get area levels
  area_levels <- c("5E", "5D", "5C", "5B", "5A", "3D", "3C", "4B", "Total")

  data <- data %>%
    dplyr::mutate(area = factor(area, levels = area_levels))


  if (sex == "M") {
    labels <- data.frame(
      species_common_name = unique(data$species_common_name),
      survey_abbrev = unique(data$survey_abbrev),
      sex = "M",
      area = rep(area_levels,
                 each = length(years)*length(seq(0, age_max, by = 1))),
      year = rep(years,
                 each = length(seq(0, age_max, by = 1)),
                 times = length(area_levels)),
      age = rep(seq(0, age_max, by = 1),
                times = length(area_levels)*length(years))
    )
  } else if (sex =="F") {
    labels <- data.frame(
      species_common_name = unique(data$species_common_name),
      survey_abbrev = unique(data$survey_abbrev),
      sex = "F",
      area = rep(area_levels,
                 each = length(years)*length(seq(0, age_max, by = 1))),
      year = rep(years,
                 each = length(seq(0, age_max, by = 1)),
                 times = length(area_levels)),
      age = rep(seq(0, age_max, by = 1),
                times = length(area_levels)*length(years))
    )
  }

  ages_all <- dplyr::right_join(data,
                                labels,
                                by = c("species_common_name", "area", "year", "survey_abbrev", "sex", "age"),
                                keep = FALSE)

  ages_all <- ages_all %>%
    dplyr::arrange(area, year, age)

  ages_all$area <- factor(ages_all$area, levels = area_levels)

  ages_all <- ages_all %>%
    dplyr::arrange(area)

  ages_all <- ages_all %>%
    dplyr::mutate(total = if_else(total == 0, NA, total))





  # Assemble placeholder rows
#  placeholder <- data[seq_along(area_levels), ] %>%
 #   dplyr::mutate(age = 0) %>%
  #  dplyr::mutate(proportion = 0) %>%
   # dplyr::mutate(total = NA_real_) %>%
    #dplyr::mutate(area = factor(area_levels, levels = area_levels))

  # Augment data
#  data <- data %>%
 #   dplyr::bind_rows(placeholder) %>%
  #  dplyr::arrange(area)

  # Plot ages ------------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    ages_all,
    ggplot2::aes(year, age)
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
      plot.title = ggplot2::element_text(vjust = -4),
      axis.text.x = ggplot2::element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
      strip.text.x = ggplot2::element_text(vjust = -1),
      legend.position = "none",
      panel.spacing = grid::unit(-0.1, "lines"),
      plot.margin = grid::unit(c(-3.5, 1, -4, 1), "mm")
    )

  if (sex == "M") {
    p1 <- p1 +
      ggplot2::labs(
      title = "Age frequencies",
      colour = "Sex",
      fill = "Sex"
    )
  } else if (sex == "F") {
    p1 <- p1 +
      ggplot2::labs(
      title = " ",
      colour = "Sex",
      fill = "Sex"
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
      ggplot2::scale_fill_manual(
        values = scales::alpha(line_col, alpha),
        breaks = c("M", "F")
      ) +
      ggplot2::scale_colour_manual(
        values = line_col,
        breaks = c("M", "F")
      ) +
      ggplot2::scale_size_area(max_size = max_size) +
      # ggplot2::geom_text(
      #   data = ages_all,
      #   y = age_max + 0.005 * age_range,
      #   mapping = ggplot2::aes(x = year, label = total),
      #   inherit.aes = FALSE,
      #   colour = "grey50",
      #   size = count_label_size,
      #   hjust = 1,
      #   angle = 90
      # ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          size = proportion,
          group = sex,
          fill = sex,
          colour = sex
        ),
        pch = 21
      ) +
      ggplot2::scale_size_continuous(range = c(0.01, 4)
      )
  }

  # Return plot ----------------------------------------------------------------

  return(p1)
}
