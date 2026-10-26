# extent ----------------------------------------------------------------
#
# This script contains R functions primarily relating to peat extent.
#
#
#

#' Create a peat extent agreement raster
#'
#' Creates an agreement raster showing the number of input datasets that
#' classify each cell as peat soil; (class 6).
#'
#' All input rasters must share the same extent, resolution, origin and
#' coordinate reference system.
#'
#' @param raster_paths Character vector of aligned raster file paths.
#' @param peat_class Integer raster value representing peat soil.
#'
#' @return A character scalar containing the output file path.
#'
create_agreement_map <- function(
    input_paths,
    peat_class
) {
  
  configure_terra()
  
  fs::dir_create(
    fs::path("data", "outputs")
  )
  
  raster_output_path <- fs::path(
    "data",
    "outputs",
    "peat_soil_agreement_map.tif"
  )
  
  s <- terra::rast(input_paths)
  
  sum(s == peat_class,
      na.rm = TRUE,
      filename = raster_output_path,
      overwrite = TRUE,
      wopt = list(
        datatype = "INT1U",
        gdal = c(
          "TILED=YES"
        )
      ))
  
  raster_output_path
  
}

