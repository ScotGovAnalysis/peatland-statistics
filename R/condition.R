# condition --------------------------------------------------------------------
# 
# This script contains R functions used to analyse peat condition.
# 
# Spatial datasets have file targets.

create_unclipped_basemap <- function(lcs_88_std,
                                     lca_std,
                                     lcs_88_condition_lookup){
  
  cli::cli_alert_info("Unclipped basemap: reading datasets - {Sys.time()}")
  
  # datasets
  
  lcs_88 <- terra::rast(lcs_88_std)
  
  lca <- terra::rast(lca_std)
  
  lookup <- lcs_88_condition_lookup
  
  # labels 
  
  labels <- terra::levels(lcs_88)[[1]]

  labels <- dplyr::full_join(
    labels,
    lookup,
    by = "DOMTEXT"
  ) |> 
    dplyr::bind_rows(
      data.frame(value = 200, # add value to facilitate uplands correction
                 DOMTEXT = "LCA uplands uorrection",
                 condition = "LCA uplands correction")
    ) |> 
    dplyr::arrange(condition) |> 
    dplyr::group_by(condition) |>
    dplyr::mutate(
      condition_value = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() 
    

  cli::cli_alert_info("Unclipped basemap: classifying - {Sys.time()}")
  
  # classify 
  
  lcs_88 <- terra::classify(
    lcs_88,
    labels |> dplyr::select(value, condition_value)
  )
  
  cli::cli_alert_info("Unclipped basemap: get uplands correction indices - {Sys.time()}")
  
  # uplands correction and na to mapping offset
  
  extensive_grassland_value <- labels |>
    dplyr::filter(condition == "extensive grassland") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_value <- labels |>
    dplyr::filter(condition == "LCA uplands correction") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_code <- terra::levels(lca)[[1]] |> 
    dplyr::filter(SqMid == "Uplands") |> 
    dplyr::pull(value) |> 
    dplyr::first()
  
  uplands_correction_indices <-
    lcs_88$DOMTEXT == extensive_grassland_value &
    lca$SqMid == uplands_code
  
  mapping_offset_value <- labels |>
    dplyr::filter(condition == "mapping offset") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  cli::cli_alert_info("Unclipped basemap: apply uplands correction - {Sys.time()}")
  
  lcs_88[uplands_correction_indices] <- uplands_correction_value
  
  cli::cli_alert_info("Unclipped basemap: NA to mapping offset - {Sys.time()}")
  
  lcs_88 <- terra::ifel(is.na(lcs_88), mapping_offset_value, lcs_88)

  cli::cli_alert_info("Unclipped basemap: update levels and crs - {Sys.time()}")
  
  # update levels
  
  levels(lcs_88) <- labels |> dplyr::select(condition_value, condition) |> 
    dplyr::distinct()
  
  # set crs
  
  crs(lcs_88) <- "EPSG:27700"
  
  cli::cli_alert_info("Unclipped basemap: saving output - {Sys.time()}")
  
  # save output
  output_path <- fs::path("data", "processed", "basemap_unclipped.tif")
  
  terra::writeRaster(
    lcs_88,
    filename = output_path,
    overwrite = TRUE,
    datatype = "INT1U",
    gdal = c(
      "COMPRESS=LZW",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Summarise peat condition by extent class and boundary
#'
#' Produces area summaries of peat condition classes within combinations of
#' boundary geography and peat depth class. The function cross-tabulates a
#' boundary raster, a peat extent/depth raster, and a peat condition raster,
#' converts cell counts to hectares, and derives cumulative peat area metrics
#' for depth thresholds of 30 cm, 40 cm, and 50 cm.
#'
#' The output contains one row per boundary, condition class, and depth
#' threshold combination, together with total peat area and total land area
#' summaries for the corresponding boundary.
#'
#' @param extent_path Character scalar. Path to a processed peat
#'   extent/depth raster. The raster is expected to contain peat depth
#'   classes coded from 0 to 6, where class 6 represents peat depths of
#'   50 cm or greater.
#' @param boundary_path Character scalar. Path to a rasterised boundary
#'   dataset produced by [rasterize_boundary()]. The raster is expected to
#'   contain a categorical `boundary_name` attribute.
#' @param condition_path Character scalar. Path to a peat condition raster
#'   whose classes are to be summarised within each peat extent class and
#'   boundary.
#'
#' @return A tibble with one row per boundary, condition category, and
#' peat depth threshold. The output contains the following variables:
#'
#' * `boundary_class`: Boundary geography type.
#' * `boundary_name`: Boundary name.
#' * `extent_source`: Source peat extent dataset.
#' * `depth_class`: Peat depth threshold (`pd_30`, `pd_40`, `pd_50`).
#' * `condition`: Peat condition class.
#' * `area_ha`: Area in hectares within the specified depth threshold and
#' condition class.
#' * `total_pd_50`: Total peat area (ha) with depth ≥ 50 cm within the
#' boundary.
#' * `total_pd_40`: Total peat area (ha) with depth ≥ 40 cm within the
#' boundary.
#' * `total_pd_30`: Total peat area (ha) with depth ≥ 30 cm within the
#' boundary.
#' * `land_area_ha`: Total land area (ha) within the boundary.
summarise_condition_crosstab <- function(extent_path, boundary_path, condition_path){
  # datasets
  extent <- terra::rast(extent_path)
  boundary <- terra::rast(boundary_path)
  condition <- terra::rast(condition_path)
  cell_area_ha <- prod(terra::res(extent)) / 10000
  
  # name of extent map and boundary
  extent_name <- extent_path |> stringr::str_remove("data/processed/") |>
    stringr::str_remove("_std.tif")
  boundary_class_name <- boundary_path |> stringr::str_remove("data/processed/") |>
    stringr::str_remove("_rast.tif")
  
  output <- terra::crosstab(c(boundary, extent, condition), long = TRUE) |> 
    mutate(area_ha = n*cell_area_ha,
           extent_source = extent_name,
           boundary_class = boundary_class_name,
           boundary_name = as.character(boundary_name),
           .keep = 'unused') |> 
    group_by(boundary_name) |>
    mutate(land_area_ha = sum(area_ha, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = peat_depth_class,
                names_prefix = "class_",
                values_from = area_ha,
                values_fill = 0)

  # Ensure columns present for each class (where missing) but that NA values provided for binary maps
  unique_depth_vals <- terra::unique(extent) |> as.vector() |> unlist()
  unique_depth_vals <- paste0("class_", unique_depth_vals)
  missing <- setdiff(paste0("class_", 0:6), names(output))

  for (nm in missing) {
    output[[nm]] <- if (nm %in% unique_depth_vals) 0 else NA
  }

  output <- output |>
    mutate(
      "pd_50" = class_6,
      "pd_40" = class_6 + class_5,
      "pd_30" = class_6 + class_5 + class_4
    ) |>
    group_by(boundary_name) |> 
    mutate(total_pd_50 = sum(pd_50),
           total_pd_40 = sum(pd_40),
           total_pd_30 = sum(pd_30)) |> 
    ungroup() |> 
    pivot_longer(
      cols = c(pd_50, pd_40, pd_30),
      names_to = "depth_class",
      values_to = "area_ha"
    ) |>
    select(
      boundary_class,
      boundary_name,
      extent_source,
      depth_class,
      condition,
      area_ha,
      total_pd_50,
      total_pd_40,
      total_pd_30,
      land_area_ha
    )

  output
}