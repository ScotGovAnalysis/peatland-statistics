# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(crew)
# Load other packages as needed.

# Set target options:

config <- yaml::read_yaml("config/config.yml")

tar_option_set(
  
  packages = c(
    "readr",
    "purrr",
    "tibble"
  ),
  
  controller = crew::crew_controller_local(
    workers = config$crew$workers,
    seconds_idle = config$crew$seconds_idle
  )
)


# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
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
    terra_options,
    config$terra_options
  ),
  
  tar_target(
    spatial_data_input_list,
    config$spatial_input_datasets
  ),
  
  tar_target(
    agreement_analysis_filepath_list,
    fs::path(
      "data",
      "processed",
      config$agreement_analysis$datasets
    ),
    format = "file"
  ),
  
  tar_target(
    agreement_analysis_peat_class,
    config$agreement_analysis$peat_class
  ),
  
  tar_target(
    agreement_analysis_boundary_path,
    fs::path(
      "data",
      "processed",
      config$agreement_analysis$boundary
    ),
    format = "file"
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
    public_data_catalogue_file,
    fs::path("config", "public_input_data_catalogue.csv"),
    format = "file"
  ),
  
  tar_target(
    public_data_catalogue,
    readr::read_csv(public_data_catalogue_file) |>
      tibble::as_tibble()
  ),
  
  # Input ----
  
  tar_target(
    download_public_datasets,
    do.call(
      download_dataset_and_metadata,
      public_data_catalogue
    ),
    pattern = map(public_data_catalogue),
    format = "file"
  ),
  
  # Processing ----
  
  tar_target(
    process_spatial_datasets,
    process_spatial_dataset(dataset = spatial_data_input_list,
                            extent_list = common_extent,
                            resolution = common_resolution,
                            terra_options = terra_options),
    pattern = map(spatial_data_input_list),
    format = "file"
  ),

  # Analysis ----
  
  tar_target(
    agreement_map,
    create_agreement_map(
      raster_paths = agreement_analysis_filepath_list,
      peat_class = agreement_analysis_peat_class,
      boundary_path = agreement_analysis_boundary_path
    ),
    format = "file"
  )

  
)
