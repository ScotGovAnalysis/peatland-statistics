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
                 Condition = "LCA Uplands Correction")
    ) |> 
    dplyr::arrange(Condition) |> 
    dplyr::group_by(Condition) |>
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
    dplyr::filter(Condition == "Extensive Grassland") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_value <- labels |>
    dplyr::filter(Condition == "LCA Uplands Correction") |>
    dplyr::pull(condition_value) |> 
    dplyr::first()
  
  uplands_correction_indices <-
    lcs_88$DOMTEXT == extensive_grassland_value &
    lca$SqMid == "Uplands"
  
  lcs_88[uplands_correction_indices] <- uplands_correction_value
  
  # update levels
  
  levels(lcs_88) <- labels |> dplyr::select(condition_value, Condition) |> 
    dplyr::distinct()
  
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

  