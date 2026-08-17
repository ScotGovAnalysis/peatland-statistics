# condition --------------------------------------------------------------------
# 
# This script contains R functions used to analyse peat condition.
# 
# Spatial datasets have file targets.

#' Create LCS88 uplands correction layer
#'
#' Reads the standardised LCS88 land cover and Landscape Character
#' Assessment (LCA) datasets, identifies LCS88 polygons that intersect
#' LCA polygons classified as "Uplands", and adds a logical `uplands`
#' field indicating whether each LCS88 polygon overlaps an upland area.
#'
#' The resulting dataset retains all LCS88 polygons and is written to a
#' GeoPackage in the processed data directory.
#'
#' @param lcs_88_std Character. Path to the standardised LCS88 spatial
#' dataset.
#' @param lca_std Character. Path to the standardised LCA spatial dataset.
#'
#' @return Character. Path to the output GeoPackage containing the LCS88
#' layer with the additional `uplands` indicator field.
create_lcs_88_uplands_correction <- function(lcs_88_std, lca_std){
  
  lcs_88 <- terra::vect(lcs_88_std)
  lca <- terra::vect(lca_std)
  
  lca <- lca[lca$SqMid == "Uplands", ]
  
  hits <- terra::relate(lcs_88, lca, relation = "intersects")
  
  lcs_88$uplands <- lengths(hits) > 0
  
  output_path <- fs::path(
    "data",
    "processed",
    "lcs_88_uplands_correction.gpkg"
  )
  
  terra::writeVector(
    lcs_88,
    filename = output_path,
    filetype = "GPKG",
    overwrite = TRUE
  )
  
  output_path
}
  