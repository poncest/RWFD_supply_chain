# global.R

# Setup ------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,        # Easily Install and Load the 'Tidyverse'
  shiny,            # Web Application Framework for R
  bslib,            # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
  DT,               # A Wrapper of the JavaScript Library 'DataTables'
  scales,           # Scale Functions for Visualization
  glue,             # Interpreted String Literals
  readxl,           # Read Excel Files
  janitor,          # Simple Tools for Examining and Cleaning Dirty Data
  ggiraph,          # Make 'ggplot2' Graphics Interactive
  assertthat,       # Easy Pre and Post Assertions
  shinycssloaders,  # Add Loading Animations to a 'shiny' Output While It's recalculating
  testthat,         # Unit Testing for R
  here              # A Simpler Way to Find Your Files
)

# Source helper functions
source("R/utils/data_prep.R")
source("R/weight_band_normalization.R")
source("R/key_metrics_validation.R")
source("R/service_level_analysis.R")
source("R/utils/download_utils.R")  

# Theme setup
source("R/utils/theme_setup.R")

# Run tests only when RUN_TESTS environment variable is set to "true"
if (Sys.getenv("RUN_TESTS") == "true") {
  message("Running tests...")
  tryCatch(
    {
      source(here::here("tests", "testthat", "setup.R"))
      testthat::test_dir(here::here("tests", "testthat"), reporter = "progress")
    },
    error = function(e) {
      message("Tests failed: ", e$message)
    }
  )
}

# run_tests.R
# Sys.setenv(RUN_TESTS = "true")  # Enable test mode

# run_app.R
# Sys.setenv(RUN_TESTS = "false")  # Disable test mode


