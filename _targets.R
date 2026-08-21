# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Load other packages as needed.

# Global config ----

global_config <- yaml::read_yaml("config/config.yml")

# Set target options:

tar_option_set(
  
  packages = c(
    "readr",
    "purrr",
    "tibble",
    "dplyr",
    "tidyr",
    "terra",
    "sf",
    "exactextractr",
    "httr2",
    "fs"
  ),

)

if (global_config$crew$use_crew) {
  tar_option_set(
    controller = crew::crew_controller_local(
      workers = global_config$crew$workers,
      seconds_idle = global_config$crew$seconds_idle
    )
  )
}

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
  
  tar_target(
    lcs_88_condition_lookup_file,
    fs::path("config", "lcs_88_condition_lookup.csv"),
    format = "file"
  ),
  
  tar_target(
    lcs_88_condition_lookup,
    readr::read_csv(lcs_88_condition_lookup_file)
  ),

  download_targets,
  verify_targets,
  
  # Processing ----
  
  processed_targets,

  tar_target_raw(
    name = "extent_targets",
    command = extent_targets_expr
  ),

  tar_target_raw(
    name = "boundary_targets",
    command = boundary_targets_expr
  ),

  tar_target(
    extent_boundary_combinations,
    tidyr::crossing(
      extent_path = extent_targets,
      boundary_path = boundary_targets
    )
  ),

  # Analysis ----

  agreement_target,

  tar_target(
    extent_analysis,
    summarise_extent_inexact(
      extent_path = extent_boundary_combinations$extent_path,
      boundary_path = extent_boundary_combinations$boundary_path
    ),
    pattern = map(extent_boundary_combinations)
  ),

  tar_target(
    extent_analysis_combined,
    dplyr::bind_rows(extent_analysis)
  ),
  
  tar_target(
    unclipped_basemap_1990,
    create_unclipped_basemap(lcs_88_std, lca_std, lcs_88_condition_lookup),
    format = "file"
  ),
  
  tar_target(
    condition_analysis,
    summarise_condition(
      extent_path = extent_boundary_combinations$extent_path,
      boundary_path = extent_boundary_combinations$boundary_path,
      condition_path = unclipped_basemap_1990
    ),
    pattern = map(extent_boundary_combinations)
  ),
  
  tar_target(
    condition_analysis_combined,
    dplyr::bind_rows(condition_analysis)
  )
  
  
)
