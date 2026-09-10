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
    "fs",
    "stringr"
  )

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
  
  tar_target_raw(
    name = "boundary_vect_targets",
    command = boundary_vect_targets_expr,
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
    baseline_condition_summary_dataset,
    dplyr::bind_rows(baseline_condition_analysis) |> 
      apply_condition_assumptions()
  ),
  
  tar_target(
    pa_spatial_data_availability,
    categorise_pa_by_spatial_data_availability(
      input_pa_non_spatial[[1]],
      pa_footprints_std,
      pa_centroids_std
    )
  ),
  
  tar_target(
    land_area,
    sf::read_sf(land_area_bdry)
  ),
  
  tar_target(
    centroids_to_buffer,
    pa_spatial_data_availability |>
    filter(spat_data_class == "centroids only") |> 
    select(grant_id, total_ha_restored) |>
    distinct() |>
    left_join(pa_centroids_std |> sf::st_read()) |> 
    filter(total_ha_restored > 0)
  ),
  
  tar_target(
    buffered_centroids,
    centroids_to_buffer |> 
      mutate(geom = create_footprint(
        geom = centroids_to_buffer$geom,
        target_ha = centroids_to_buffer$total_ha_restored,
        clip_geom = land_area
      )) |> sf::st_as_sf(),
    pattern = map(centroids_to_buffer)
  ),
  
  tar_target(
    pa_buffered_centroids_combined,
    dplyr::bind_rows(buffered_centroids) |> 
      left_join(pa_spatial_data_availability)
  ),
  
  tar_target(
    pa_footprints_formatted,
    format_footprints(
        pa_spatial_data_availability,
        pa_footprints_std,
        land_area)
  ),
  
  tar_target(
    combined_pa_dataset,
    create_combined_pa_dataset(
      pa_spatial_data_availability,
      pa_footprints_formatted,
      pa_buffered_centroids_combined),
    format = "file"
  ),
  
  tar_target(
    pa_sf,
    sf::st_read(combined_pa_dataset, quiet = TRUE)
  ),
  
  tar_target(
    pa_by_boundary,
    summarise_pa_by_boundary(
      pa = pa_sf,
      boundary_path = boundary_vect_targets
    ),
    pattern = map(boundary_vect_targets)
  ),
  
  tar_target(
    pa_land_area,
    summarise_pa_land_area(
      pa = pa_sf
    )
  ),
  
  tar_target(
    restoration_summary_dataset,
    dplyr::bind_rows(pa_land_area,
                     pa_by_boundary)
  ),
  
  combined_rewetting_target,
  
  tar_target(
    rewetting_sf,
    sf::st_read(combined_rewetting_dataset) 
  ),
  
  tar_target(
    rewetting_by_boundary,
    summarise_rewetting_by_boundary(
      rewetting_sf,
      boundary_vect_targets
    ),
    pattern = map(boundary_vect_targets)
  ),
  
  tar_target(
    rewetting_land_area,
    summarise_rewetting_land_area(
      rewetting_sf,
      input_non_spatial_rewetting
    )
  ),
  
  tar_target(
    rewetting_summary_dataset,
    dplyr::bind_rows(rewetting_by_boundary,
                     rewetting_land_area)
  ),
  
  tar_target(
    simplified_condition_time_series_dataset,
    create_simple_condition_ts(
      baseline_condition_summary_dataset,
      rewetting_summary_dataset)
  ),

  tar_target(
    output_datasets,
    write_output_datasets(
      rewetting_summary_dataset,
      restoration_summary_dataset,
      baseline_condition_summary_dataset,
      simplified_condition_time_series_dataset
    ),
    format = "file"
  )
  

)
