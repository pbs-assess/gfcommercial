# Load packages
library(targets)
library(tarchetypes)

# Set targets options
tar_option_set(
  packages = c(
    "csasdown",
    "here"
  )
)

# Set options
options(
  # clustermq.scheduler = "multicore",
  tidyverse.quiet = TRUE
)

# Source R/
suppressWarnings(tar_source())

# List targets
list(
  # Species name nodes ---------------------------------------------------------
  list(
    tar_target(
      species,
      readr::read_csv(here::here("data", "species.csv")),
      format = "file"
    ),
    tar_target(
      species_common_name,
      as.list(species$common_name)
    ),
    tar_targets(
      species_file_name,
      as.list(species$file_name)
    ),
  ),
  # Define data nodes ----------------------------------------------------------
  list(
    # Define external data path ------------------------------------------------
    tar_target(
      data_cache_path,
      here::here("data-cache")
    ),
    # TODO: Read species data files as nodes
    tar_map(
      values = species,
      tar_target(
        species_data,
        readr::read_rds(paste0(data_cache_path, species_file_name, ".rds")),
        format = "file"
      ),
      list()
    ),
    list()
  ),
  # Render report --------------------------------------------------------------
  tar_target(
    tech_report,
    csasdown::render(verbose = TRUE),
    format = "file"
  ),
  list()
)
