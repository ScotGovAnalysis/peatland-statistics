# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
# tar_option_set(
#   packages = c("tibble") # Packages that your targets need for their tasks.
#   # format = "qs", # Optionally set the default storage format. qs is fast.
#   #
#   # Pipelines that take a long time to run may benefit from
#   # optional distributed computing. To use this capability
#   # in tar_make(), supply a {crew} controller
#   # as discussed at https://books.ropensci.org/targets/crew.html.
#   # Choose a controller that suits your needs. For example, the following
#   # sets a controller that scales up to a maximum of two workers
#   # which run as local R processes. Each worker launches when there is work
#   # to do and exits if 60 seconds pass with no tasks to run.
#   #
#   #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
#   #
#   # Alternatively, if you want workers to run on a high-performance computing
#   # cluster, select a controller from the {crew.cluster} package.
#   # For the cloud, see plugin packages like {crew.aws.batch}.
#   # The following example is a controller for Sun Grid Engine (SGE).
#   #
#   #   controller = crew.cluster::crew_controller_sge(
#   #     # Number of workers that the pipeline can scale up to:
#   #     workers = 10,
#   #     # It is recommended to set an idle time so workers can shut themselves
#   #     # down if they are not running tasks.
#   #     seconds_idle = 120,
#   #     # Many clusters install R as an environment module, and you can load it
#   #     # with the script_lines argument. To select a specific verison of R,
#   #     # you may need to include a version string, e.g. "module load R/4.3.2".
#   #     # Check with your system administrator if you are unsure.
#   #     script_lines = "module load R"
#   #   )
#   #
#   # Set other options as needed.
# )

tar_option_set(
  
  packages = c(
    "readr",
    "purrr",
    "tibble"
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
      config$agreement_analysis_datasets
    )
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
    public_data_catalogue,
    readr::read_csv(fs::path("config", "public_input_data_catalogue.csv")) |>
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
      raster_paths = agreement_analysis_filepath_list
    ),
    format = "file"
  )

  
)
