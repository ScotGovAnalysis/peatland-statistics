# download_data ----------------------------------------------------------------
#
# This script contains R functions used to download publicly available data.
#
# The URLs for datasets and associated metadata should be specified in the file
# "public_input_data_catalogue.csv" in the config folder.


download_dataset <- function(
    dataset,
    data_location,
    dataset_format,
    metadata_location,
    metadata_format
) {
  
  dataset_folder_path <- path("data", "raw", dataset)
  
  dataset_file_path <- path(
    dataset_folder_path,
    paste0(dataset, dataset_format)
  )
  
  metadata_file_path <- path(
    dataset_folder_path,
    paste0(dataset, "_metadata", metadata_format)
  )
  
  dir_create(dataset_folder_path)
  
  data_location |>
    request() |>
    req_perform(path = dataset_file_path) |> 
    print()
  
  metadata_location |>
    request() |>
    req_perform(path = metadata_file_path) |> 
    print()
  
}
