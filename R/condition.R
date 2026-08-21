# condition --------------------------------------------------------------------
# 
# This script contains R functions used to analyse peat condition.
# 
# Spatial datasets have file targets.

create_unclipped_basemap <- function(lcs_88_std,
                                     lca_std,
                                     lcs_88_condition_lookup){
  
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
    

  # classify 
  
  lcs_88 <- terra::classify(
    lcs_88,
    labels |> dplyr::select(value, condition_value)
  )
  
  # uplands correction
  
  extensive_grassland_value <- labels |>
    dplyr::filter(condition == "extensive grassland") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_value <- labels |>
    dplyr::filter(condition == "LCA uplands correction") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_indices <-
    lcs_88$DOMTEXT == extensive_grassland_value &
    lca$SqMid == "Uplands"
  
  lcs_88[uplands_correction_indices] <- uplands_correction_value
  
  # update levels
  
  levels(lcs_88) <- labels |> dplyr::select(condition_value, condition) |> 
    dplyr::distinct()
  
  # set na values in condition map to 'mapping offset'
  mapping_offset_val <- labels |>
    dplyr::filter(condition == "mapping offset") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  lcs_88 <- terra::ifel(is.na(lcs_88), mapping_offset_val, lcs_88)
  
  # set crs
  
  crs(lcs_88) <- "EPSG:27700"
  
  # save output
  output_path <- fs::path("data", "processed", "basemap_unclipped.tif")
  
  terra::writeRaster(
    lcs_88,
    filename = output_path,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=None",
      "TILED=YES"
    )
  )
  
  output_path
}


summarise_condition <- function(extent_path,
                                boundary_path,
                                condition_path) {
  # datasets
  extent <- terra::rast(extent_path)
  geometry <- sf::st_read(boundary_path) |>
    mutate(bdry_area_ha = sf::st_area(geom) |> as.numeric() / 10000)
  condition <- terra::rast(condition_path)
  
  # set na values in condition map to 'mapping offset'
  mapping_offset_val <- levels(condition)[[1]] |> 
    filter(condition == "mapping offset") |> 
    pull(value) |> 
    first()
  
  condition <- terra::ifel(is.na(condition), mapping_offset_val, condition)
  
  # combined peat depth and condition classes
  combined <- 100 * extent + condition
  names(combined) <- "combined_class"
  
  # name of extent map
  extent_name <- extent_path |> stringr::str_remove("data/processed/") |>
    stringr::str_remove("_std.tif")
  
  # condition coding
  levels <- terra::levels(condition)[[1]] |>
    rename("condition_int" = "value")
  
  # fractional coverage
  frac <- exactextractr::exact_extract(
    x = combined,
    y = geometry,
    fun = "frac",
    progress = TRUE,
    append_cols = c("boundary_class", "boundary_name", "bdry_area_ha")
  ) |>
    tibble::as_tibble()
  
  output <- frac |> tidyr::pivot_longer(cols = dplyr::starts_with("frac")) |>
  mutate(
    name = name |> stringr::str_remove("frac_") |> as.integer(),
    depth_int = as.integer(name %/% 100),
    condition_int = as.integer(name %% 100),
    area_ha = value * bdry_area_ha
  ) |>
    mutate(extent_source = extent_name) |>
    tidyr::pivot_wider(
      names_from = depth_int,
      values_from = area_ha,
      names_prefix = "class_"
    )
  
  for (nm in paste0("class_", 0:6)) {
    if (!nm %in% names(output)) {
      output[[nm]] <- 0
    }
  }

  output <- output |>
    mutate(
      "pd_50" = class_6,
      "pd_40" = sum(class_6, class_5, na.rm = TRUE),
      "pd_30" = sum(class_6, class_5, class_4, na.rm = TRUE)
    ) |>
    pivot_longer(
      cols = c(pd_50, pd_40, pd_30),
      names_to = "depth_class",
      values_to = "area_ha"
    ) |>
    mutate(area_ha = tidyr::replace_na(area_ha, 0)) |>
    full_join(y = levels) |>
    select(
      boundary_class,
      boundary_name,
      extent_source,
      depth_class,
      condition,
      area_ha,
      bdry_area_ha
    )

  output
}

summarise_condition_inexact <- function(extent_path,
                                boundary_path,
                                condition_path) {
  # datasets
  extent <- terra::rast(extent_path)
  geometry <- sf::st_read(boundary_path)
  condition <- terra::rast(condition_path)
  cell_area_ha <- prod(terra::res(extent)) / 10000
  
  # combined peat depth and condition classes
  combined <- 100 * extent + condition
  names(combined) <- "combined_class"
  
  # name of extent map
  extent_name <- extent_path |> stringr::str_remove("data/processed/") |>
    stringr::str_remove("_std.tif")
  
  # condition coding
  levels <- terra::levels(condition)[[1]] |>
    rename("condition_int" = "value")
  
  # cell counts
  output <- terra::extract(combined, geometry, fun = "table", touches = FALSE, bind = TRUE) |> 
    as.data.frame() |> 
    pivot_longer(cols = starts_with("count"),
                 names_to = "peat_class",
                 values_to = "count") |> 
    mutate(
      class_code = peat_class |> stringr::str_remove("count\\.") |> as.integer(),
      depth_class = paste0("class_", as.integer(class_code %/% 100)),
      condition_int = as.integer(class_code %% 100),
      area_ha = count * cell_area_ha,
      extent_source = extent_name,
    ) |> 
    select(-class_code, -peat_class, -count) |> 
    group_by(boundary_name) |> # must be unique!
    mutate(land_area_ha = sum(area_ha)) |>
    ungroup() |>
    pivot_wider(values_from = area_ha,
                names_from = depth_class,
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
      "pd_40" = sum(class_6, class_5, na.rm = FALSE),
      "pd_30" = sum(class_6, class_5, class_4, na.rm = FALSE)
    ) |>
    pivot_longer(
      cols = c(pd_50, pd_40, pd_30),
      names_to = "depth_class",
      values_to = "area_ha"
    ) |>
    full_join(y = levels) |>
    select(
      boundary_class,
      boundary_name,
      extent_source,
      depth_class,
      condition,
      area_ha,
      land_area_ha
    )
  
  output
}