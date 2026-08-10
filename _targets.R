# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(crew)
library(tarchetypes)
# Load other packages as needed.

# Global config ----

global_config <- yaml::read_yaml("config/config.yml")

# Set target options:

tar_option_set(
  
  packages = c(
    "readr",
    "purrr",
    "tibble"
  ),
  
  controller = crew::crew_controller_local(
    workers = global_config$crew$workers,
    seconds_idle = global_config$crew$seconds_idle
  )
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:

# Simple targets are defined in the list below - more complex targets are defined
# in configure_pipeline.R

list(
  
  # Configuration ----
  
  tar_target(
    config_file,
    fs::path("config", "config.yml"),
    format = "file"
  ),

  tar_target(
    config,
    yaml::read_yaml(config_file)
  ),
  
  tar_target(
    common_extent,
    config$spatial_parameters$common_extent
  ),

  tar_target(
    common_resolution,
    config$spatial_parameters$common_resolution
  ),
  
  tar_target(
    agreement_analysis_peat_class,
    config$agreement_analysis$peat_class
  ),

  # Input ----
  
  tar_target(
    public_data_catalogue_file,
    fs::path("config", "public_input_data_catalogue.csv"),
    format = "file"
  ),

  tar_target(
    public_data_catalogue,
    readr::read_csv(public_data_catalogue_file) |>
      tibble::as_tibble()
  ),
  
  download_targets,
  verify_targets,
  
  # Processing ----
  
  processed_targets,
  
  # Analysis ----
  
  agreement_target
  
)
