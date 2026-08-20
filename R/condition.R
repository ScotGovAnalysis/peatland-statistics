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
                 DOMTEXT = "LCA Uplands Correction",
                 condition = "LCA Uplands Correction")
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
    dplyr::filter(condition == "Extensive Grassland") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_value <- labels |>
    dplyr::filter(condition == "LCA Uplands Correction") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_indices <-
    lcs_88$DOMTEXT == extensive_grassland_value &
    lca$SqMid == "Uplands"
  
  lcs_88[uplands_correction_indices] <- uplands_correction_value
  
  # update levels
  
  levels(lcs_88) <- labels |> dplyr::select(condition_value, condition) |> 
    dplyr::distinct()
  
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
      "pd50" = class_6,
      "pd40" = sum(class_6, class_5, na.rm = TRUE),
      "pd30" = sum(class_6, class_5, class_4, na.rm = TRUE)
    ) |>
    pivot_longer(
      cols = c(pd50, pd40, pd30),
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
