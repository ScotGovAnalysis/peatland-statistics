# processing -------------------------------------------------------------------
#
# This script contains R functions used to process input datasets. Each input 
# dataset has a unique processing function.
#
# Spatial datasets may be standardised to a common extent, resolution, CRS or
# data format.
#
# Spatial datasets have a file target.
#
# Input datasets should be specified in config/config.yml .

# terra options ----------------------------------------------------------------

#' Set global terra options
#'
#' Configures memory management options for the current R session using
#' `terra::terraOptions()`.
#'
#' @param terra_options A named list containing terra configuration options.
#' Must contain elements `memfrac` and `memmax`.
#'
#' @return Invisibly returns NULL
set_terra_options <- function(terra_options) {
  terra::terraOptions(memfrac = terra_options$memfrac,
                      memmax = terra_options$memmax)
  invisible(NULL)
}

# dispatch ---------------------------------------------------------------------

#' Process a spatial dataset
#'
#' Dispatches to the appropriate dataset-specific processing function based on
#' the supplied dataset name.
#'
#' @param dataset_name A character scalar identifying the dataset to process.
#' @param extent_list A list specifying xmin, xmax, ymin, ymax
#'
#' @return A character vector of file paths created by the processing function.
#' Intended for use with `targets` file targets (`format = "file"`)


process_spatial_dataset <- function(dataset_name,
                                    extent_list,
                                    resolution,
                                    terra_options){
  
  set_terra_options(terra_options)
  
  switch(
    dataset_name,
    aitkenhead_19_pd = process_aitkenhead_19_pd(extent_list,
                                                resolution),
    gagkas_24_psum = process_gagkas_24_psum(extent_list,
                                                  resolution),
    robb_25_pd = process_robb_25_pd(extent_list,
                                                  resolution),
    stop("No processor defined for ", dataset_name)
  )
}

# spatial helper functions -----------------------------------------------------

#' Create a SpatExtent from a named list
#'
#' Converts a list containing `xmin`, `xmax`, `ymin`, and `ymax`
#' elements into a `terra::SpatExtent`.
#'
#' @param x A named list with elements `xmin`, `xmax`, `ymin`, and `ymax`.
#'
#' @return A `terra::SpatExtent`.
#'
extent_from_list <- function(x) {
  terra::ext(
    x$xmin,
    x$xmax,
    x$ymin,
    x$ymax
  )
}

#' Standardise raster resolution
#'
#' Aggregates or disaggregates a raster to a target resolution. Continuous
#' rasters are aggregated using the mean, whilst categorical rasters are
#' aggregated using the modal value. Disaggregation is performed using nearest
#' neighbour assignment.
#'
#' This function assumes that the source and target grids share a common CRS
#' and origin, and that the ratio between the source and target resolutions is
#' an integer multiple.
#'
#' @param r A `terra::SpatRaster`.
#' @param resolution Numeric. Target cell resolution in map units.
#' @param categorical Logical. Should the raster be treated as categorical?
#' Defaults to `FALSE`.
standardise_res <- function(r, resolution, categorical = FALSE) {
  
  res_fact <- resolution / terra::res(r)[1]
  
  if (res_fact > 1) {
    
    terra::aggregate(
      r,
      fact = round(res_fact),
      fun = if (categorical) "modal" else "mean"
    )
    
  } else if (res_fact < 1) {
    
    terra::disagg(
      r,
      fact = round(1 / res_fact),
      method = "near"
    )
    
  } else {
    r
  }
}

#' Standardise raster extent
#'
#' Crops and extends a raster to a common extent. Raster cells falling outside
#' the target extent are removed, and areas within the target extent but not
#' covered by the input raster are filled with `NA`.
#'
#' This function assumes that the raster has already been aligned to the
#' desired grid and is typically applied before resolution standardisation to
#' ensure consistent grid alignment.
#'
#' @param r A `terra::SpatRaster`.
#' @param common_extent A named list defining the target extent. This is
#' converted to a `terra::SpatExtent` using `extent_from_list()`.
#'
#' @return A `terra::SpatRaster` with the requested extent.
standardise_ext <- function(r, common_extent) {
  
  extent <- extent_from_list(common_extent)
  
  r |>
    terra::crop(extent) |>
    terra::extend(extent)
  
}

#' Discretise peat depth values into categorical classes
#'
#' Reclassifies a peat depth raster into integer classes according to the
#' following scheme:
#'
#' 0 = depth of 0,
#' 1 = depth from 0 to less than 10,
#' 2 = depth from 10 to less than 20,
#' 3 = depth from 20 to less than 30,
#' 4 = depth from 30 to less than 40,
#' 5 = depth from 40 to less than 50,
#' 6 = depth of 50 or greater.
#'
#' Class intervals are left-closed and right-open (`[a, b)`).
#'
#' @param x A `terra::SpatRaster` containing peat depth values.
#'
#' @return A `terra::SpatRaster` containing peat depth classes represented
#' by the integers 0 to 6.
discretise_peat_depth <- function(x){
  
  terra::ifel(x == 0, 0,
    terra::classify(
      x,
      rbind(c(0, 10, 1),
        c(10, 20, 2),
        c(20, 30, 3),
        c(30, 40, 4),
        c(40, 50, 5),
        c(50, Inf, 6)
      ),
      right = FALSE # intervals left closed, right open [a,b)
    )
  )
  
}


# spatial dataset processing ---------------------------------------------------

#' Process the Aitkenhead (2019) peat depth dataset
#'
#' Extracts the source raster from a ZIP archive, standardises to common extent
#' and resolution, discretises peat depth, and writes the result to a 
#' compressed GeoTIFF.
#' 
#' Original resolution: 100 m
#'
#' The output raster uses the following peat depth classes:
#' 0 = depth of 0,
#' 1 = depth from 0 to less than 10,
#' 2 = depth from 10 to less than 20,
#' 3 = depth from 20 to less than 30,
#' 4 = depth from 30 to less than 40,
#' 5 = depth from 40 to less than 50,
#' 6 = depth of 50 or greater.
#'
#' The output is stored as an unsigned 8-bit integer raster and compressed
#' using ZSTD compression.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
process_aitkenhead_19_pd <- function(extent_list, resolution) {
  
  zip_file_path <- fs::path("data",
                            "raw",
                            "aitkenhead_19_pd",
                            "aitkenhead_19_peat_depth.zip")
  
  extract_dir <- fs::dir_create(
    fs::path(tempdir(), "aitkenhead_19_peat_depth")
  )
  
  utils::unzip(
    zipfile = zip_file_path,
    exdir = extract_dir
  )
  
  r <- terra::rast(
    fs::path(extract_dir, "peat_depth.tif")
  ) |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution) |> 
    discretise_peat_depth()
  
  output_path <- fs::path("data", "processed", "aitkenhead_19_pd_std.tif")
  
  fs::dir_create(
    fs::path("data", "processed")
  )
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process the Gagkas and Lilly (2024) peat depth dataset
#'
#' Standardises to common extent and resolution, discretises peat depth, 
#' and writes the result to a compressed GeoTIFF.
#' 
#' Original resolution: 50 m
#'
#' The output raster uses the following peat depth classes:
#' 0 = depth of 0,
#' 6 = depth of 50 or greater.
#'
#' The output is stored as an unsigned 8-bit integer raster and compressed
#' using ZSTD compression.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
process_gagkas_24_psum <- function(extent_list, resolution) {
  
  input_file_path <- fs::path("data",
                            "raw",
                            "gagkas_24_psum",
                            "DSM_Peat_m1_Psum.tif")
  
  r <- terra::rast(
    input_file_path
  ) 
    
  r <- terra::ifel(r == 1, 50, 0)
  
  r <- r |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution) |> 
    discretise_peat_depth()
  
  output_path <- fs::path("data", "processed", "gagkas_24_psum_std.tif")
  
  fs::dir_create(
    fs::path("data", "processed")
  )
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process the Robb et al., 2025 peat depth dataset
#'
#' Standardises to common extent and resolution, discretises peat depth values
#' into categorical classes and writes the result to a compressed GeoTIFF.
#' 
#' Original resolution: 10 m
#'
#' The output raster uses the following peat depth classes:
#' 0 = depth of 0,
#' 1 = depth from 0 to less than 10,
#' 2 = depth from 10 to less than 20,
#' 3 = depth from 20 to less than 30,
#' 4 = depth from 30 to less than 40,
#' 5 = depth from 40 to less than 50,
#' 6 = depth of 50 or greater.
#'
#' The output is stored as an unsigned 8-bit integer raster and compressed
#' using ZSTD compression.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
process_robb_25_pd <- function(extent_list, resolution) {
  
  input_file_path <- fs::path("data",
                            "raw",
                            "robb_25_pd",
                            "PeatThickness.tif")
  
  r <- terra::rast(
    input_file_path
  ) |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution) |> 
    discretise_peat_depth()
  
  output_path <- fs::path("data", "processed", "robb_25_pd_std.tif")
  
  fs::dir_create(
    fs::path("data", "processed")
  )
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=NONE",
      "TILED=YES"
    )
  )
  
  output_path
}

