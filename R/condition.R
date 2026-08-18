# condition --------------------------------------------------------------------
# 
# This script contains R functions used to analyse peat condition.
# 
# Spatial datasets have file targets.

create_unclipped_basemap <- function(lcs_88_std,
                                     lca_std,
                                     lcs_88_condition_lookup){
  
  lcs_88 <- terra::rast(lcs_88_std)
  
  lookup <- lcs_88_condition_lookup
  
  labels <- terra::levels(lcs_88)[[1]]

  labels <- dplyr::full_join(
    labels,
    lookup,
    by = "DOMTEXT"
  ) |> 
    dplyr::arrange(Condition) |> 
    dplyr::group_by(Condition) |>
    dplyr::mutate(
      condition_value = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup()

  lcs_88 <- terra::classify(
    lcs_88,
    labels |> dplyr::select(value, condition_value)
  )
  
  levels(lcs_88) <- labels |> dplyr::select(condition_value, Condition) |> 
    dplyr::distinct()
  
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

  