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
  # Define external data path --------------------------------------------------
  tar_target(
    data_cache_path,
    here::here("data-cache")
  ),
  # Render report --------------------------------------------------------------
  tar_target(
    tech_report,
    csasdown::render(verbose = TRUE),
    format = "file"
  ),
  list()
)
