# processing -------------------------------------------------------------------
#
# This script contains R functions used to process input datasets. Each input 
# dataset has a unique processing function.
#
# Spatial datasets have a file target.
#
# Input datasets should be specified in config/config.yml .


# dispatch ---------------------------------------------------------------------

#' Process a spatial dataset
#'
#' Dispatches to the appropriate dataset-specific processing function based on
#' the supplied dataset name.
#'
#' @param dataset_name A character scalar identifying the dataset to process.
#'
#' @return A character vector of file paths created by the processing function.
#' Intended for use with `targets` file targets (`format = "file"`)


process_spatial_dataset <- function(dataset_name){
  
  switch(
    dataset_name,
    aitkenhead_19_peat_depth = process_aitkenhead_19_peat_depth(),
    gagkas_lilly_24 = process_gagkas_lilly_24(),
    robb_et_al_25_peat_depth = process_robb_et_al_25_peat_depth(),
    stop("No processor defined for ", dataset_name)
  )
}

# spatial helper functions -----------------------------------------------------

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
#' Extracts the source raster from a ZIP archive, discretises peat depth values
#' into categorical classes, extends the raster to a common Scotland-wide
#' extent, and writes the result to a compressed GeoTIFF.
#' 
#' Resolution: 100 m
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

process_aitkenhead_19_peat_depth <- function() {
  
  zip_file_path <- fs::path("data",
                            "raw",
                            "aitkenhead_19_peat_depth",
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
  )
  
  r <- discretise_peat_depth(r) |> 
    terra::extend(terra::ext(0, 500000, 500000, 1300000)) # common extent
  
  output_path <- fs::path("data", "processed", "aitkenhead_19_peat_depth_rast_cat_100.tif")
  
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

