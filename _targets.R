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

# tar_source("other_functions.R") # Source other scripts as needed.
# Run the R scripts in the R/ folder with your custom functions:
tar_source()

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
    public_data_catalogue_validation,
    validate_public_data_catalogue_datasets(
      config,
      public_data_catalogue
    )
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
  arcgol_api_targets,
  
  # Processing ----
  
  processed_targets,
  
  boundary_rast_processing_targets,
  
  tar_target(
    hex_grid_10km,
    process_hex_grid_10km(land_area_bdry),
    format = "file"
  ),

  tar_target_raw(
    name = "extent_targets",
    command = extent_targets_expr,
    iteration = "list"
  ),

  tar_target_raw(
    name = "boundary_rast_targets",
    command = boundary_rast_targets_expr,
    iteration = "list"
  ),

  # Analysis ----

  agreement_target,

  tar_target(
    unclipped_basemap,
    create_unclipped_basemap(lcs_88_std, lca_std, lcs_88_condition_lookup),
    format = "file"
  ),
  
  tar_target(
    baseline_condition_analysis,
    summarise_condition_crosstab(
      extent_path = extent_targets,
      boundary_path = boundary_rast_targets,
      condition_path = unclipped_basemap
    ),
    pattern = cross(extent_targets, boundary_rast_targets)
  ),
  
  tar_target(
    baseline_condition_analysis_combined,
    dplyr::bind_rows(baseline_condition_analysis)
  )

)
