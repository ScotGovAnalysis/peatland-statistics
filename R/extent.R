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
    raster_paths,
    peat_class = 6
) {
  
  output_path <- fs::path(
    "data",
    "outputs",
    "peat_soil_agreement_map.tif"
  )
  
  s <- terra::rast(raster_paths)

  agree <- terra::app(
    s,
    fun = \(x) sum(x == peat_class, na.rm = TRUE)
  )
  
  fs::dir_create(
    fs::path("data", "outputs")
  )
  
  terra::writeRaster(
    agree,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES",
      "BIGTIFF=IF_SAFER"
    )
  )
  
  output_path
  
}
