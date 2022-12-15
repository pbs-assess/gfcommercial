# Load packages
library(targets)

# Set targets options
tar_option_set(
  packages = c(
    "csasdown"
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
  # Render report --------------------------------------------------------------
  list(
    tar_target(
      tech_report,
      csasdown::render(verbose = TRUE),
      format = "file"
    )
  ),
  list()
)
