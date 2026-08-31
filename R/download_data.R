# download_data ----------------------------------------------------------------
#
# This script contains R functions used to download publicly available data.
#
# The URLs for datasets and associated metadata should be specified in the file
# "public_input_data_catalogue.csv" in the config folder.

validate_public_data_catalogue_datasets <- function(config, public_data_catalogue) {
  
  config_datasets <- tibble::enframe(
    config$input_datasets,
    name = "input_dataset_name",
    value = "metadata"
  ) |>
    tidyr::unnest_wider(metadata) |> 
    filter(fun %in% c("download", "argol_api")) |> 
    pull(input_dataset_name)
  
  catalogue_datasets <- public_data_catalogue$dataset
  
  missing <- setdiff(
    config_datasets,
    catalogue_datasets
  )
  
  if (length(missing) > 0) {
    stop(
      "Datasets missing from catalogue: ",
      paste(missing, collapse = ", ")
    )
  }
  
  TRUE
}


download_dataset <- function(
    input_dataset_name,
    public_data_catalogue){
  
  dataset_args <-
    public_data_catalogue |>
    dplyr::filter(dataset == input_dataset_name)
  
  if (nrow(dataset_args) == 0) {
    stop(
      sprintf(
        "Dataset '%s' not found in public_data_catalogue.",
        input_dataset_name
      )
    )
  }
  
  if (nrow(dataset_args) > 1) {
    stop(
      sprintf(
        "Dataset '%s' appears %s times in public_data_catalogue.",
        input_dataset_name,
        nrow(dataset_args)
      )
    )
  }
  
  dataset_args <- as.list(dataset_args[1, ])
  
  do.call(
    download_dataset_and_metadata,
    dataset_args
  )
  
}

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

#' Download metadata for a dataset
#'
#' Downloads a metadata file and saves it to the dataset folder.
#'
#' @param metadata_location Character. URL or file location of the metadata.
#' @param dataset Character. Dataset name used in messages and output filename.
#' @param dataset_folder_path Character. Directory where the metadata file
#' should be saved.
#' @param metadata_extension Character. File extension for the metadata file,
#' including the leading dot (e.g. ".xml", ".html").
#'
#' @return Character path to the downloaded metadata file, or `NULL` if no
#' metadata was available or the download failed.
#'
#' @details
#' If `metadata_location` is `NA` or an empty string, no download is attempted.
#' Download failures are caught and reported via `cli`.
download_metadata <- function(
    metadata_location,
    dataset,
    dataset_folder_path,
    metadata_extension
) {
  
  if (is.na(metadata_location) || metadata_location == "") {
    
    cli::cli_alert_info(
      "No metadata available for '{dataset}'"
    )
    
    return(NULL)
  }
  
  metadata_file_path <- fs::path(
    dataset_folder_path,
    paste0(dataset, "_metadata", metadata_extension)
  )
  
  tryCatch({
    
    download_file(
      metadata_location,
      metadata_file_path
    )
    
    cli::cli_alert_success(
      "Downloaded metadata for '{dataset}'"
    )
    
    metadata_file_path
    
  }, error = \(e) {
    
    cli::cli_alert_warning(
      "Metadata download failed for '{dataset}': {e$message}"
    )
    
    NULL
  })
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
  
  metadata_file_path <- download_metadata(
    metadata_location,
    dataset,
    dataset_folder_path,
    metadata_extension
  )
  
  if (!is.null(metadata_file_path)) {
    downloaded_files <- c(
      downloaded_files,
      metadata = metadata_file_path
    )
  }
  
  downloaded_files
}

#' Verify that an input dataset file exists
#'
#' Constructs the expected path to a raw input dataset file and checks
#' that it exists. If the file is not found, an error is raised. If the
#' file exists, a success message is displayed and the file path is returned.
#'
#' @param input_dataset_name A character string specifying the dataset
#' subdirectory within `data/raw`.
#' @param filename A character string containing the name of the file to
#' verify.
#'
#' @return A file path object corresponding to the verified file location.
#'
verify_dataset <- function(input_dataset_name, filename) {
  
  path <- fs::path("data", "raw", input_dataset_name, filename)
  
  if (!fs::file_exists(path)) {
    cli::cli_abort(
      "File not found: {.file {path}}"
    )
  }
  
  cli::cli_alert_success(
    "Verified {.file {path}}"
  )
  
  path
}

#' Download a dataset from an ArcGIS Online API source
#'
#' Looks up a dataset in the public data catalogue and downloads it using
#' [`download_arcgol_api_and_metadata()`]. The catalogue must contain exactly
#' one matching entry for the requested dataset.
#'
#' @param input_dataset_name Character scalar. Name of the dataset to download.
#' @param public_data_catalogue Data frame containing dataset download
#' parameters and metadata locations.
#'
#' @return Named character vector of successfully downloaded file paths.
#'
#' @details
#' The function validates that the dataset exists in the catalogue and that
#' only a single matching record is present. The corresponding catalogue row
#' is converted to a list and supplied to
#' [`download_arcgol_api_and_metadata()`].
#'
download_arcgol_api_dataset <- function(
    input_dataset_name,
    public_data_catalogue){
  
  dataset_args <-
    public_data_catalogue |>
    dplyr::filter(dataset == input_dataset_name)
  
  if (nrow(dataset_args) == 0) {
    stop(
      sprintf(
        "Dataset '%s' not found in public_data_catalogue.",
        input_dataset_name
      )
    )
  }
  
  if (nrow(dataset_args) > 1) {
    stop(
      sprintf(
        "Dataset '%s' appears %s times in public_data_catalogue.",
        input_dataset_name,
        nrow(dataset_args)
      )
    )
  }
  
  dataset_args <- as.list(dataset_args[1, ])
  
  do.call(
    download_arcgol_api_and_metadata,
    dataset_args
  )
  
}

#' Download an ArcGIS Online dataset and associated metadata
#'
#' Downloads a dataset from an ArcGIS Online service using
#' `arcgislayers::arc_read()` and saves it to a dataset-specific folder under
#' `data/raw/`.
#'
#' The output format depends on `dataset_extension`:
#' * `"api_non_spat"`: saved as an `.rds` file.
#' * `"api_vect"`: saved as a GeoPackage (`.gpkg`).
#' * `"api_rast"`: saved as a GeoTIFF (`.tif`).
#'
#' Metadata is downloaded separately using
#' [`download_metadata()`], where available.
#'
#' @param dataset Character scalar. Dataset name used to create output folders
#' and filenames.
#' @param data_location Character scalar. ArcGIS service URL used as the data
#' source.
#' @param dataset_extension Character scalar. Indicator of the dataset type.
#' Supported values are `"api_non_spat"`, `"api_vect"`, and `"api_rast"`.
#' @param metadata_location Character scalar. URL of the metadata file to
#' download. May be `NA` or an empty string if no metadata is available.
#' @param metadata_extension Character scalar. Metadata file extension, e.g.
#' `".xml"` or `".html"`.
#'
#' @return Named character vector of successfully downloaded file paths.
#'
#' @details
#' Non-spatial datasets are saved as serialized R objects (`.rds`), vector
#' datasets are converted to `terra` vectors and written as GeoPackages, and
#' raster datasets are written as GeoTIFF files.
#'
download_arcgol_api_and_metadata <- function(
    dataset,
    data_location,
    dataset_extension,
    metadata_location,
    metadata_extension
){
  
  dataset_folder_path <- fs::path("data", "raw", dataset)
  
  fs::dir_create(dataset_folder_path)

  dataset_file_path <- dataset_folder_path

  if (dataset_extension == "api_non_spat"){

  dataset_file_path <- fs::path(
      dataset_file_path,
      paste0(dataset,".rds"))
  
  dat <- arcgislayers::arc_read(data_location)
  
  saveRDS(
    object = dat,
    file = dataset_file_path
  )
  
  
  } else if (dataset_extension == "api_vect"){
    
    dataset_file_path <- fs::path(
      dataset_file_path,
      paste0(dataset,".gpkg"))
    
      arcgislayers::arc_read(data_location) |> 
      terra::vect() |> 
      terra::writeVector(
        filename = dataset_file_path, 
        overwrite = TRUE
      )
    
  } else if(dataset_extension == "api_rast"){
    dataset_file_path <- fs::path(
      dataset_file_path,
      paste0(dataset,".tif"))
    
    arcgislayers::arc_read(data_location) |> 
      terra::writeRaster(
        filename = dataset_file_path,
        overwrite = TRUE
      )
  }
  
  downloaded_files <- c(dataset = dataset_file_path)
  
  metadata_file_path <- download_metadata(
    metadata_location,
    dataset,
    dataset_folder_path,
    metadata_extension
  )
  
  if (!is.null(metadata_file_path)) {
    downloaded_files <- c(
      downloaded_files,
      metadata = metadata_file_path
    )
  }

  downloaded_files
}

