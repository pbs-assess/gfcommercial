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
