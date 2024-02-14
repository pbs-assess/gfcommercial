get_spp_names <- function(file = NULL) {

  spp <- utils::read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE,
                         comment.char = "#")
  #spp <- dplyr::filter(spp, !is.na(species_code))
  #spp$species_code <- sprintf(paste0("%0", 3L, "d"), spp$species_code)
  spp$species_common_name <- tolower(gsub(" $", "", spp$species_common_name))

  # Not in databases at all:
  spp <- spp[!duplicated(spp), , drop = FALSE]
  spp$spp_w_hyphens <- gsub("/", "-", gsub(" ", "-", spp$species_common_name))
  stopifnot(sum(duplicated(spp$species_common_name)) == 0)
  tibble::as_tibble(spp)
}


# Function to make species science names in italics ----------------------------
emph <- function(x) paste0("\\emph{", x, "}")


# Function to split up the scientific names for Rougheye -----------------------
rougheye_split <- function(x) {
  spl <- strsplit(x, "/")[[1]]
  first <- strsplit(spl, " ")[[1]][[1]]
  second <- strsplit(spl, " ")[[1]][[2]]
  third <- strsplit(spl, " ")[[2]][[1]]
  c(paste(first, second, sep = "-"), paste(first, third, sep = "-"))
}

# Assign areas function --------------------------------------------------------

assign_areas <- function(major_stat_area_description,
                         area_regex = c("3[CD]+", "5[AB]+", "5[CDE]+")) {
  out <- rep(NA, length(major_stat_area_description))
  for (i in seq_along(area_regex)) {
    out[grepl(area_regex[i], major_stat_area_description)] <-
      gsub("\\^|\\[|\\]|\\+", "", area_regex[i])
  }
  out
}

# Round to any denomination - from gfplot
round_any <- function(x, accuracy) {
  round(x / accuracy) * accuracy
}

# Round nice - from gfplot
round_nice <- function(x, thousands_k = TRUE) {
  out <- round_any(x, 100)
  out[out == 0] <- x[out == 0]
  if (thousands_k) {
    out <- as.numeric(out)
    out <- ifelse(out >= 1000, numform::f_thous(out, relative = 0L), out)
    # out <- gsub("\\.0K", "K", out)
  }
  out[x == 0] <- ""
  out
}

# Round catch values to nearest kg, t, or kt
catch_rounding <- function(value,
                           units = NULL,
                           ...) {

  if (is.null(units)) {
    units <- c(`kt` = 1000000,`t` = 1000, `kg` = 1)
  }

  scale_val <- units[[1]]

  for (i in seq_along(units)) {
    if (value < (1000 * units[[i]])) {
      scale <- units[[i]]
      SIunit <- names(units)[units == units[[i]]]
    }
  }
  if (SIunit == "kt") {
    scale_val <- round_any(value / scale, 0.1)
  } else {
    scale_val <- round_any(value / scale, 1)
  }
  text <- paste0(scale_val, SIunit)
  text[value == 0] <- ""
  text
}


# Adopted from gfplot
bin_lengths <- function(dat, value, bin_size) {
  value <- enquo(value)
  bin_range <- dat %>%
    select(!!value) %>%
    pull() %>%
    range()
  bin_range[1] <- round_down_even(bin_range[1])
  bin_range[2] <- ceiling(bin_range[2])
  bins <- seq(min(bin_range), max(bin_range), by = bin_size)
  mutate(dat, !!quo_name(value) :=
           bins[findInterval(!!value, bins)] + bin_size / 2)
}

# Adopted from gfplot
round_down_even <- function(x, base = 2) {
  base * floor(x / base)
}

