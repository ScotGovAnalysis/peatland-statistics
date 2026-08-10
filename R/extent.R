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
    peat_class = 6,
    boundary_path
) {
  
  
  s <- terra::rast(input_paths)

  message("Creating agreement raster")
  
  agree <- terra::app(
    s == peat_class,
    fun = sum,
    na.rm = TRUE
  )
  
  fs::dir_create(
    fs::path("data", "outputs")
  )
  
  boundary <- terra::vect(boundary_path)
  
  agree <- terra::mask(agree, boundary)
  
  message("Converting to vector")
  agree_vect <- terra::as.polygons(
    agree,
    dissolve = TRUE,
    na.rm = TRUE
  )
  
  message("Saving outputs")
  
  raster_output_path <- fs::path(
    "data",
    "outputs",
    "peat_soil_agreement_map.tif"
  )
  
  vector_output_path <- fs::path(
    "data",
    "outputs",
    "peat_soil_agreement_map.gpkg"
  )
  
  terra::writeRaster(
    agree,
    filename = raster_output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES",
      "BIGTIFF=IF_SAFER"
    )
  )
  
  terra::writeVector(
    agree_vect,
    filename = vector_output_path,
    overwrite = TRUE
  )
  
  c(raster_output_path, vector_output_path)
  
}

#' Summarise categorical raster areas within geometries
#'
#' Uses exact extraction to calculate the area of each raster category
#' within one or more geometries. Results are returned in wide format with
#' one row per geometry and one column per raster class.
#'
#' @param raster A single-layer categorical SpatRaster.
#' @param geometry An sf, sfc, or SpatVector object containing the
#'   geometries to summarise.
#'
#' @return A tibble with one row per geometry and columns named
#'   `class_<value>` containing areas hectares

summarise_categories <- function(raster, geometry) {
  
  frac <- exactextractr::exact_extract(
    x = raster,
    y = geometry,
    fun = "frac",
    progress = TRUE
  )
  
  area_ha <- sf::st_area(geometry, unit = "ha") |> 
    as.numeric()
  
  output <- frac |> mutate(
    across(everything(), ~ .x * area_ha)
  ) |> 
    rename_with(.fn = ~stringr::str_replace(.x, "frac", "class")) |> 
    tibble::rownames_to_column(var = "feature_id") |> 
    pivot_longer(cols = -feature_id,
                 names_to = "class",
                 values_to = "area_ha")
  
  
  output
}
