#' Get spp. names
#' Adapted from gfsynopsis
#'
#' @export
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
