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


# spatial helper functions -----------------------------------------------------

#' Extract a ZIP archive to a temporary directory
#'
#' Creates a unique temporary directory, extracts the contents of a ZIP archive
#' into it, and returns the path to the extraction directory.
#'
#' @param zipfile Character scalar. Path to the ZIP archive.
#'
#' @return A character scalar giving the path to the directory containing the
#' extracted files.
#'
unzip_to_temp <- function(zipfile) {
  extract_dir <- fs::dir_create(
    tempfile(pattern = "unzip_", tmpdir = tempdir())
  )
  
  utils::unzip(
    zipfile = zipfile,
    exdir = extract_dir
  )
  
  extract_dir
}

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

#' Categorise peat depth values into discrete depth classes
#'
#' Reclassifies a peat depth raster into integer depth classes. Missing
#' values and depths equal to zero are assigned to class `0`. Positive
#' depth values are classified according to the following scheme:
#'
#' * 0 = missing values and depth of 0
#' * 1 = depth from 0 to less than 10
#' * 2 = depth from 10 to less than 20
#' * 3 = depth from 20 to less than 30
#' * 4 = depth from 30 to less than 40
#' * 5 = depth from 40 to less than 50
#' * 6 = depth of 50 or greater
#'
#' Class intervals are left-closed and right-open (`[a, b)`).
#'
#' @param x A `terra::SpatRaster` containing peat depth values.
#'
#' @return A `terra::SpatRaster` containing integer peat depth classes
#' from 0 to 6.
discretise_peat_depth <- function(x) {
  terra::ifel(is.na(x), 0,
              terra::ifel(x == 0, 0, 
                          terra::classify(
                            x,
                            rbind(
                              c(0, 10, 1),
                              c(10, 20, 2),
                              c(20, 30, 3),
                              c(30, 40, 4),
                              c(40, 50, 5),
                              c(50, Inf, 6)
                            ),
                            right = FALSE # intervals left closed, right open [a,b)
                          )))
}


# spatial dataset processing ---------------------------------------------------

#' Process the Aitkenhead (2019) peat depth dataset
#'
#' Extracts the source raster from a ZIP archive, standardises it to a
#' common spatial extent and resolution, categorises peat depth values
#' using [discretise_peat_depth()] and writes the result to a GeoTIFF.
#'
#' Original resolution: 100 m.
#'
#' The output raster contains peat depth classes represented by the
#' integers 0 to 6, where higher values correspond to greater peat depth.
#'
#' The output is stored as an unsigned 8-bit integer raster with tiled
#' storage.
#'
#' @param source_path Path to the ZIP archive containing the source peat
#'   depth raster.
#' @param extent_list A list defining the target spatial extent passed to
#'   [standardise_ext()].
#' @param resolution Target raster resolution passed to
#'   [standardise_res()].
#'
#' @return A character scalar giving the path to the processed raster file.
#'   Intended for use with `targets` file targets (`format = "file"`).
process_aitkenhead_19_pd_std <- function(source_path, extent_list, resolution){
  
  extract_dir <- unzip_to_temp(source_path)
  
  r <- terra::rast(
    fs::path(extract_dir, "peat_depth.tif")
  ) |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution) |> 
    discretise_peat_depth()
  
  output_path <- fs::path("data", "processed", "aitkenhead_19_pd_std.tif")
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=None",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process the Gagkas and Lilly (2024) peat soil dataset
#'
#' Reads the source raster, converts peat soil presence values to a nominal
#' peat depth of 50 cm and all other values to 0 cm, standardises the raster
#' to a common spatial extent and resolution, categorises values using
#' [discretise_peat_depth()] and writes the result to a GeoTIFF.
#'
#' Original resolution: 50 m.
#'
#' The output raster contains peat depth classes represented by the
#' integers 0 to 6, where higher values correspond to greater peat depth.
#'
#' The output is stored as an unsigned 8-bit integer raster with tiled
#' storage.
#'
#' @param source_path Path to the source raster file.
#' @param extent_list A list defining the target spatial extent passed to
#'   [standardise_ext()].
#' @param resolution Target raster resolution passed to
#'   [standardise_res()].
#'
#' @return A character scalar giving the path to the processed raster file.
#'   Intended for use with `targets` file targets (`format = "file"`).
#'   
process_gagkas_24_psum_std <- function(source_path, extent_list, resolution) {
  
  r <- terra::rast(source_path) 
    
  r <- terra::ifel(is.na(r), 0,
                   terra::ifel(r == 1, 50, 0))
  
  r <- r |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution) |> 
    discretise_peat_depth()
  
  output_path <- fs::path("data", "processed", "gagkas_24_psum_std.tif")
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=None",
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
#' The output raster contains peat depth classes represented by the
#' integers 0 to 6, where higher values correspond to greater peat depth.
#'
#' The output is stored as an unsigned 8-bit integer raster with tiled storage.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
process_robb_25_pd <- function(source_path, extent_list, resolution) {
  
  r <- terra::rast(
    source_path
  ) |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution) |> 
    discretise_peat_depth()
  
  output_path <- fs::path("data", "processed", "robb_25_pd_std.tif")
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=None",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process Intermediate Zone boundary dataset
#'
#' Extracts the Intermediate Zone boundary dataset from a ZIP archive,
#' reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_class` and `boundary_name`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_int_dzs_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "SG_IntermediateZoneBdry_2022_MHW.shp")
  )
  
  v$boundary_class <- "intermediate_datazone"
  v$boundary_name <- v$IZName
  
  v <- v[, c("boundary_class", "boundary_name")]
  
  
  
  output_path <- fs::path("data", "processed", "int_dzs.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Process Scotland land area boundary
#'
#' Extracts the Intermediate Zone 2022 boundary dataset, dissolves all
#' boundaries into a single land area polygon, attaches standard boundary
#' metadata, and writes the result to a GeoPackage.
#' 
#' The 'land area' is described here: https://www.spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/2978ed67-dade-42ec-b8e1-644e0b1f8cd8
#' This is the area to mean high water, excluding inland water bodies greater than 1 square kilometre in area.
#'
#' @param source_path Character vector or list containing the path to the
#' downloaded boundary ZIP file in the first element.
#'
#' @return Character scalar giving the path to the output GeoPackage.
#'
process_land_area_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "SG_IntermediateZoneBdry_2022_MHW.shp")
  )
  
  v <- terra::aggregate(v)
  
  v$boundary_class <- "land area - MHW"
  v$boundary_name <- "land area - MHW"
  
  v <- v[, c("boundary_class", "boundary_name")]
  
  output_path <- fs::path("data", "processed", "land_area.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Process local authority boundary dataset
#'
#' Extracts the local authority boundary dataset from a ZIP archive,
#' reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_class` and `boundary_name`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_las_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "pub_las.shp")
  )
  
  v$boundary_class <- "local_authority"
  v$boundary_name <- v$local_auth
  
  v <- v[, c("boundary_class", "boundary_name")]
  
  output_path <- fs::path("data", "processed", "las.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}