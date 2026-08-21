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
    peat_class = 6
) {
  
  
  s <- terra::rast(input_paths)
  
  agree <- terra::app(
    s == peat_class,
    fun = sum,
    na.rm = TRUE
  )
  
  fs::dir_create(
    fs::path("data", "outputs")
  )
  
  raster_output_path <- fs::path(
    "data",
    "outputs",
    "peat_soil_agreement_map.tif"
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
  
  raster_output_path
  
}

#' Summarise habitat extent within boundary geometries
#'
#' Reads an extent raster and a boundary dataset, calculates the area of
#' each raster class within each boundary using exact extraction, and
#' derives summary areas for the `pd30`, `pd40`, and `pd50` depth classes.
#' Results are returned in long format with one row per boundary and depth
#' class combination.
#'
#' @param extent_path Path to a categorical extent raster.
#' @param boundary_path Path to a vector boundary dataset readable by
#' [sf::st_read()].
#'
#' @return A tibble with the following columns:
#' 
#' boundary_class - High level boundary classification.
#' boundary_name - Boundary name
#' extent_source - Name of the peat extentsource.
#' depth_class - One of `pd_30`, `pd_40`, or `pd_50`.
#' area_ha - Area within the boundary, in hectares.
#' bdry_area_ha - Total area of the boundary
summarise_extent <- function(extent_path, boundary_path) {
  
  raster <- terra::rast(extent_path)
  geometry <- sf::st_read(boundary_path)
  
  cell_area_ha <- prod(terra::res(raster)) / 10000
  
  extent_name <- extent_path |> stringr::str_remove("data/processed/") |>
    stringr::str_remove("_std.tif")
  
  frac <- exactextractr::exact_extract(
    x = raster,
    y = geometry,
    fun = function(df) df |> group_by(value) |> summarise(sum_frac = sum(coverage_fraction, na.rm = TRUE)),
    summarize_df = TRUE,
    progress = TRUE,
    append_cols = c("boundary_class", "boundary_name")
  ) |> 
    tibble::as_tibble() 
  
   output <- frac |> 
    mutate(area_ha = sum_frac * cell_area_ha,
           extent_source = extent_name,
           .keep = "unused") |>
    filter(!is.na(value)) |> # remove non-land areas
    group_by(boundary_class, boundary_name) |>
    mutate(land_area_ha = sum(area_ha)) |>
    ungroup() |>
    pivot_wider(values_from = area_ha,
                names_from = value,
                names_prefix = "class_")

  for (nm in paste0("class_", 0:6)) {
    if (!nm %in% names(output)) {
      output[[nm]] <- 0
    }
  }

  output <- output |>
    mutate("pd_50" = class_6,
           "pd_40" = class_6 + class_5,
           "pd_30" = class_6 + class_5 + class_4) |>
  pivot_longer(cols = c(pd_50, pd_40, pd_30),
               names_to = "depth_class",
               values_to = "area_ha") |>
    mutate(extent_source = extent_name,
           area_ha = tidyr::replace_na(area_ha, 0)) |>
    select(boundary_class, boundary_name, extent_source, depth_class, area_ha,
           land_area_ha)
  
  
  output
}

summarise_extent_inexact <- function(extent_path, boundary_path) {
  
  raster <- terra::rast(extent_path)
  geometry <-terra::vect(boundary_path)
  
  cell_area_ha <- prod(terra::res(raster)) / 10000
  
  extent_name <- extent_path |> stringr::str_remove("data/processed/") |>
    stringr::str_remove("_std.tif")
  
  output <- terra::extract(raster, geometry, fun = "table", touches = FALSE, bind = TRUE) |> 
    as.data.frame() |> 
    pivot_longer(cols = starts_with("count"),
                 names_to = "depth_class",
                 values_to = "count") |> 
    mutate(depth_class = depth_class |> stringr::str_replace("count\\.", "class_")) |> 
    mutate(area_ha = count * cell_area_ha,
           extent_source = extent_name,
           .keep = "unused") |>
    group_by(boundary_name) |> # must be unique!
    mutate(land_area_ha = sum(area_ha)) |>
    ungroup() |>
    pivot_wider(values_from = area_ha,
                names_from = depth_class,
                values_fill = 0)
  
  # Ensure columns present for each class (where missing) but that NA values provided for binary maps
  unique_depth_vals <- terra::unique(raster) |> as.vector() |> unlist()
  unique_depth_vals <- paste0("class_", unique_depth_vals)
  missing <- setdiff(paste0("class_", 0:6), names(output))
  
  for (nm in missing) {
    output[[nm]] <- if (nm %in% unique_depth_vals) 0 else NA
  }

  output <- output |>
    mutate("pd_50" = class_6,
           "pd_40" = class_6 + class_5,
           "pd_30" = class_6 + class_5 + class_4) |>
    pivot_longer(cols = c(pd_50, pd_40, pd_30),
                 names_to = "depth_class",
                 values_to = "area_ha") |>
    mutate(extent_source = extent_name) |>
    select(boundary_class, boundary_name, extent_source, depth_class, area_ha,
           land_area_ha)
  
  
  output
}
