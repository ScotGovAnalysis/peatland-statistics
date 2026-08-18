# configure pipeline -----------------------------------------------------------
# This script contains code used to configure the main analytical pipeline, in
# _targets.R.
# 
# Many targets use static branching: 
# https://books.ropensci.org/targets/static.html

# Ensure directories exist for data

fs::path("data", "raw") |> fs::dir_create()
fs::path("data", "processed") |> fs::dir_create()
fs::path("data", "outputs") |> fs::dir_create()

# Download and verify ---

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

# Processing ----

processed_target_values <- tibble::enframe(
  global_config$processed_datasets,
  name = "processed_dataset_name",
  value = "metadata"
) |>
  tidyr::unnest_wider(metadata)

processed_targets <- purrr::pmap(
  processed_target_values,
  function(processed_dataset_name, source_dataset, type) {
    
    SOURCE = as.name(paste0("input_", source_dataset))
    
    PROCESSOR = as.name(paste0("process_", processed_dataset_name))
    
    switch(type,
           # extent
           extent = tar_target_raw(
             name = processed_dataset_name,
             command = substitute(
               PROCESSOR(
                 source_path = SOURCE,
                 extent_list = common_extent,
                 resolution = common_resolution
               ),
             ),
             format = "file"
           ),
           # boundary
           boundary = tar_target_raw(
             name = processed_dataset_name,
             command = substitute(
               PROCESSOR(
                 source_path = SOURCE
               ),
             ),
             format = "file"
           ),
           # condition
           condition = tar_target_raw(
             name = processed_dataset_name,
             command = substitute(
               PROCESSOR(
                 source_path = SOURCE,
                 extent_list = common_extent,
                 resolution = common_resolution
               ),
             ),
             format = "file"
           )
    )
  }
)


# Agreement map ----

agreement_input_expr <- as.call(
  c(
    as.name("c"),
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

# Extent ----

extent_targets_expr <- tibble::enframe(
  global_config$processed_datasets,
  name = "processed_dataset_name",
  value = "metadata"
) |>
  tidyr::unnest_wider(metadata) |>
  dplyr::filter(type == "extent") |>
  dplyr::pull(processed_dataset_name) |>
  lapply(as.name) |>
  (\(x) as.call(c(as.name("c"), x)))()


boundary_targets_expr <- tibble::enframe(
  global_config$processed_datasets,
  name = "processed_dataset_name",
  value = "metadata"
) |>
  tidyr::unnest_wider(metadata) |>
  dplyr::filter(type == "boundary") |>
  dplyr::pull(processed_dataset_name) |>
  c("unclipped_bdry") |> 
  lapply(as.name) |>
  (\(x) as.call(c(as.name("c"), x)))()


