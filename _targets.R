# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(crew)
library(tarchetypes)
# Load other packages as needed.

# Set target options:

global_config <- yaml::read_yaml("config/config.yml")

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

# Define static branches - https://books.ropensci.org/targets/static.html

input_target_values <-
  tibble::enframe(
    global_config$input_datasets,
    name = "input_dataset_name",
    value = "metadata"
  ) |>
  tidyr::unnest_wider(metadata)

download_target_values <-
  input_target_values |>
  dplyr::filter(fun == "download")

verify_target_values <-
  input_target_values |>
  dplyr::filter(fun == "verify")

download_targets <- tar_map(
  values = download_target_values,
  names = input_dataset_name,
  tar_target(
    input,
    download_dataset(
      input_dataset_name,
      public_data_catalogue
    ),
    format = "file"
  )
)

verify_targets <- tar_map(
  values = verify_target_values,
  names = input_dataset_name,
  tar_target(
    input,
    verify_dataset(input_dataset_name,
                   filename),
    format = "file"
  )
)

processed_target_values <- tibble::enframe(
  global_config$processed_datasets,
  name = "processed_dataset_name",
  value = "metadata"
) |>
  tidyr::unnest_wider(metadata)

processed_targets <- purrr::pmap(
  processed_target_values,
  function(processed_dataset_name, source_dataset, type) {
    
    tar_target_raw(
      name = processed_dataset_name,
      command = substitute(
        PROCESSOR(
          source_path = SOURCE,
          extent_list = common_extent,
          resolution = common_resolution
        ),
        list(
          PROCESSOR = as.name(
            paste0("process_", processed_dataset_name)
          ),
          SOURCE = as.name(
            paste0("input_", source_dataset)
          )
        )
      ),
      format = "file"
    )
    
  }
)

agreement_input_expr <- as.call(
  c(
    list(as.name("c")),
    lapply(
      global_config$agreement_analysis$input_datasets,
      as.name
    )
  )
)

agreement_target <- tar_target_raw(
  name = "agreement_map",
  command = bquote(
    create_agreement_map(
      input_paths = .(agreement_input_expr),
      peat_class = agreement_analysis_peat_class,
      boundary_path = int_dzs_bdry
    )
  ),
  format = "file"
)

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
