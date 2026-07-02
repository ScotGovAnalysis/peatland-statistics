# download_data ----------------------------------------------------------------
#
# This script contains R functions used to download publicly available data.
#
# The URLs for datasets and associated metadata should be specified in the file
# "public_input_data_catalogue.csv" in the config folder.



#' Download a file
#'
#' Helper function for download_dataset_and_metadata
#' Downloads a file specified by URL to local file path.
#'
#' @param url Character scalar. A valid URL.
#' @param path Character scalar. Local file path where the content will be saved.
#'
#' @return An httr2 response object.
#'
download_file <- function(url, path) {
  url |>
    httr2::request() |>
    httr2::req_perform(path = path)
}


#' Download a dataset and associated metadata
#'
#' Downloads a dataset and, where available, its associated metadata from the
#' supplied URLs. Files are saved to a dataset-specific folder under
#' `data/raw/`.
#'
#' Metadata is optional. If `metadata_location` is `NA` or an empty string,
#' no metadata download is attempted.
#'
#' This function is intended for use with `purrr::pmap()`, where each row of a
#' parameter table supplies the arguments for a single dataset download.
#'
#' @param dataset Character scalar. Dataset name used to create the output
#'   folder and file names.
#' @param data_location Character scalar. URL of the dataset file to download.
#' @param dataset_extension Character scalar. Dataset file extension, e.g.
#'   `".zip"` or `".csv"`.
#' @param metadata_location Character scalar. URL of the metadata file to
#'   download. May be `NA` or an empty string if no metadata is available.
#' @param metadata_extension Character scalar. Metadata file extension, e.g.
#'   `".xml"`.
#'
#' @return Named character vector of successfully downloaded file paths.
#'
download_dataset_and_metadata <- function(
    dataset,
    data_location,
    dataset_extension,
    metadata_location,
    metadata_extension
) {
  
  dataset_folder_path <- fs::path("data", "raw", dataset)
  
  fs::dir_create(dataset_folder_path)
  
  downloaded_files <- character()
  
  dataset_file_path <- fs::path(
    dataset_folder_path,
    paste0(dataset, dataset_extension)
  )
  
  tryCatch({
    
    download_file(data_location, dataset_file_path)
    
    cli::cli_alert_success(
      "Downloaded dataset for '{dataset}'"
    )
    
    downloaded_files <- c(
      downloaded_files,
      dataset = dataset_file_path
    )
    
  }, error = \(e) {
    
    cli::cli_alert_warning(
      "Dataset download failed for '{dataset}': {e$message}"
    )
  })
  
  if (is.na(metadata_location) || metadata_location == "") {
    
    cli::cli_alert_info(
      "No metadata available for '{dataset}'"
    )
    
  } else {
    
    metadata_file_path <- fs::path(
      dataset_folder_path,
      paste0(dataset, "_metadata", metadata_extension)
    )
    
    tryCatch({
      
      download_file(metadata_location, metadata_file_path)
      
      cli::cli_alert_success(
        "Downloaded metadata for '{dataset}'"
      )
      
      downloaded_files <- c(
        downloaded_files,
        metadata = metadata_file_path
      )
      
    }, error = \(e) {
      
      cli::cli_alert_warning(
        "Metadata download failed for '{dataset}': {e$message}"
      )
    })
  }
  
  downloaded_files
}
