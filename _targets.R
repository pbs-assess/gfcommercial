# Load packages
library(targets)
library(tarchetypes)

# Set targets options
tar_option_set(
  packages = c(
    "csasdown",
    "here",
    "tibble"
  )
)

# Set options
options(
  # clustermq.scheduler = "multicore",
  tidyverse.quiet = TRUE
)

# Source R/
lapply(list.files("R", full.names = TRUE), source)
suppressWarnings(tar_source())

file <- here::here("data", "species.csv")
species <- get_spp_names(file)
species_tibble <- tibble::as_tibble(species$spp_w_hyphens)

# List targets
list(
  # Species name nodes ---------------------------------------------------------
  # list(
  #   tar_target(
  #     file,
  #     here::here("data", "species.csv"),
  #     format = "file"
  #   ),
  #   tar_target(
  #     species,
  #     get_spp_names(file)
  #   ),
  #   tar_target(
  #     species_tibble,
  #     as_tibble(species$spp_w_hyphens)
  #   )
  # ),

  # Define data nodes ----------------------------------------------------------

    # Define external data path ------------------------------------------------
 list(
    tar_target(
      data_cache_path,
      here::here("data-cache")
    ),
    # Read species data files as nodes
    tar_map(
      values = species_tibble,
      tar_target(
      species_data,
      readr::read_rds(paste0(data_cache_path, "/", value, ".rds"))
      )
    )
  )
)



# Render report --------------------------------------------------------------
#  tar_target(
#    tech_report,
#    csasdown::render(verbose = TRUE),
#    format = "file"
#  ),


# Check for errors
#tar_manifest(fields = command)

# View dependency graph
#tar_visnetwork()

# Run the targets
#tar_make()
