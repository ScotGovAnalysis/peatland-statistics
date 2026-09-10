# processing -------------------------------------------------------------------
#
# This script contains R functions used to process input datasets. Each input 
# dataset has a unique processing function.
#
# Spatial datasets may be standardised to a common extent, resolution, CRS or
# data format.
#
# Spatial datasets have a file target.
#
# Input datasets should be specified in config/config.yml .


# spatial helper functions -----------------------------------------------------

#' Extract a ZIP archive to a temporary directory
#'
#' Creates a unique temporary directory, extracts the contents of a ZIP archive
#' into it, and returns the path to the extraction directory.
#'
#' @param zipfile Character scalar. Path to the ZIP archive.
#'
#' @return A character scalar giving the path to the directory containing the
#' extracted files.
#'
unzip_to_temp <- function(zipfile) {
  extract_dir <- fs::dir_create(
    tempfile(pattern = "unzip_", tmpdir = tempdir())
  )
  
  utils::unzip(
    zipfile = zipfile,
    exdir = extract_dir
  )
  
  extract_dir
}

#' Create a SpatExtent from a named list
#'
#' Converts a list containing `xmin`, `xmax`, `ymin`, and `ymax`
#' elements into a `terra::SpatExtent`.
#'
#' @param x A named list with elements `xmin`, `xmax`, `ymin`, and `ymax`.
#'
#' @return A `terra::SpatExtent`.
#'
extent_from_list <- function(x) {
  terra::ext(
    x$xmin,
    x$xmax,
    x$ymin,
    x$ymax
  )
}

#' Standardise raster resolution
#'
#' Aggregates or disaggregates a raster to a target resolution. Continuous
#' rasters are aggregated using the mean, whilst categorical rasters are
#' aggregated using the modal value. Disaggregation is performed using nearest
#' neighbour assignment.
#'
#' This function assumes that the source and target grids share a common CRS
#' and origin, and that the ratio between the source and target resolutions is
#' an integer multiple.
#'
#' @param r A `terra::SpatRaster`.
#' @param resolution Numeric. Target cell resolution in map units.
#' @param categorical Logical. Should the raster be treated as categorical?
#' Defaults to `FALSE`.
standardise_res <- function(r, resolution, categorical = FALSE) {
  
  res_fact <- resolution / terra::res(r)[1]
  
  if (res_fact > 1) {
    
    terra::aggregate(
      r,
      fact = round(res_fact),
      fun = if (categorical) "modal" else "mean"
    )
    
  } else if (res_fact < 1) {
    
    terra::disagg(
      r,
      fact = round(1 / res_fact),
      method = "near"
    )
    
  } else {
    r
  }
}

#' Standardise raster extent
#'
#' Crops and extends a raster to a common extent. Raster cells falling outside
#' the target extent are removed, and areas within the target extent but not
#' covered by the input raster are filled with `NA`.
#'
#' This function assumes that the raster has already been aligned to the
#' desired grid and is typically applied before resolution standardisation to
#' ensure consistent grid alignment.
#'
#' @param r A `terra::SpatRaster`.
#' @param common_extent A named list defining the target extent. This is
#' converted to a `terra::SpatExtent` using `extent_from_list()`.
#'
#' @return A `terra::SpatRaster` with the requested extent.
standardise_ext <- function(r, common_extent) {
  
  extent <- extent_from_list(common_extent)
  
  r |>
    terra::crop(extent) |>
    terra::extend(extent)
  
}

#' Categorise peat depth values into discrete depth classes
#'
#' Reclassifies a peat depth raster into integer depth classes. Missing
#' values and depths equal to zero are assigned to class `0`. Positive
#' depth values are classified according to the following scheme:
#'
#' * 0 = missing values and depth of 0
#' * 1 = depth from 0 to less than 10
#' * 2 = depth from 10 to less than 20
#' * 3 = depth from 20 to less than 30
#' * 4 = depth from 30 to less than 40
#' * 5 = depth from 40 to less than 50
#' * 6 = depth of 50 or greater
#'
#' Class intervals are left-closed and right-open (`[a, b)`).
#'
#' @param x A `terra::SpatRaster` containing peat depth values.
#'
#' @return A `terra::SpatRaster` containing integer peat depth classes
#' from 0 to 6.
discretise_peat_depth <- function(x) {
  terra::ifel(x == 0, 0, 
              terra::classify(
                x,
                rbind(
                  c(0, 10, 1),
                  c(10, 20, 2),
                  c(20, 30, 3),
                  c(30, 40, 4),
                  c(40, 50, 5),
                  c(50, Inf, 6)
                ),
                right = FALSE # intervals left closed, right open [a,b)
              ))
}

#' Apply a land area mask to a raster
#'
#' Masks a raster using a pre-rasterised land area mask. Cells outside the
#' land area are assigned `NA`, while any `NA` values within the land area
#' are replaced with `0`.
#'
#' @param extent_raster A `terra::SpatRaster` to be masked.
#' @param land_area_path Character scalar. Path to a rasterised land area
#' mask, where non-`NA` cells define the land area extent.
#'
#' @return A `terra::SpatRaster` constrained to the specified land area, with
#' internal `NA` values replaced by `0`.
apply_land_area_mask <- function(extent_raster, land_area_path){
  land_area_bdry <- terra::rast(land_area_path)
  result <- terra::cover(
    terra::mask(extent_raster, land_area_bdry),
    land_area_bdry * 0
  )
}


# spatial dataset processing ---------------------------------------------------

#' Process the Aitkenhead and Coull (2020) peat depth dataset
#'
#' Extracts the source raster from a ZIP archive, standardises it to a
#' common spatial extent and resolution, classifies peat depth values using
#' [discretise_peat_depth()], applies a Scotland land area mask, and writes
#' the result to a GeoTIFF.
#'
#' Original resolution: 100 m.
#'
#' The output raster contains peat depth classes represented by integer
#' values from 0 to 6, where higher values correspond to greater peat depth.
#' Cells outside the Scotland land area boundary are assigned `NA`, and any
#' missing values within the land area are replaced with `0`.
#'
#' The output is written as an unsigned 8-bit integer raster with ZSTD
#' compression and tiled storage.
#'
#' @param source_path Character scalar. Path to the ZIP archive containing
#' the source peat depth raster.
#' @param extent_list List defining the target spatial extent passed to
#' [standardise_ext()].
#' @param resolution Numeric scalar. Target raster resolution passed to
#' [standardise_res()].
#' @param land_area_path Character scalar. Path to a rasterised Scotland
#' land area mask used to constrain the output extent.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
process_aitkenhead_20_pd_std <- function(source_path, extent_list, resolution,
                                         land_area_path){
  
  extract_dir <- unzip_to_temp(source_path)
  
  r <- terra::rast(
    fs::path(extract_dir, "peat_depth.tif")
  ) |> 
    standardise_ext(extent_list) |> 
    discretise_peat_depth() |> 
    standardise_res(resolution, categorical = TRUE) |> 
    apply_land_area_mask(land_area_path)
  
  names(r) <- "peat_depth_class"
  
  output_path <- fs::path("data", "processed", "aitkenhead_20_pd_std.tif")
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process the Gagkas and Lilly (2024) peat soil dataset
#'
#' Reads the source raster, converts peat soil presence values to a nominal
#' peat depth class, standardises the raster to a common spatial extent and
#' resolution, applies a Scotland land area mask, and writes the result to a
#' GeoTIFF.
#'
#' Original resolution: 50 m.
#'
#' Source cells indicating peat soil presence (`1`) are assigned peat depth
#' class `6`, while all other cells are assigned peat depth class `0`.
#'
#' The output raster contains peat depth classes represented by integer
#' values from 0 to 6, where higher values correspond to greater peat depth.
#' Cells outside the Scotland land area boundary are assigned `NA`, and any
#' missing values within the land area are replaced with `0`.
#'
#' The output is written as an unsigned 8-bit integer raster with ZSTD
#' compression and tiled storage.
#'
#' @param source_path Character scalar. Path to the source raster file.
#' @param extent_list List defining the target spatial extent passed to
#' [standardise_ext()].
#' @param resolution Numeric scalar. Target raster resolution passed to
#' [standardise_res()].
#' @param land_area_path Character scalar. Path to a rasterised Scotland
#' land area mask used to constrain the output extent.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
process_gagkas_24_psum_std <- function(source_path, extent_list, resolution,
                                       land_area_path) {
  
  r <- terra::rast(source_path) 
    
  r <- terra::ifel(r == 1, 6L, 0L)
  
  r <- r |> 
    standardise_ext(extent_list) |> 
    standardise_res(resolution, categorical = TRUE) |> 
    apply_land_area_mask(land_area_path)
  
  names(r) <- "peat_depth_class"
  
  output_path <- fs::path("data", "processed", "gagkas_24_psum_std.tif")
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process the Robb et al. (2025) peat depth dataset
#'
#' Reads the source raster, standardises it to a common spatial extent and
#' resolution, classifies peat depth values using
#' [discretise_peat_depth()], applies a Scotland land area mask, and writes
#' the result to a GeoTIFF.
#'
#' Original resolution: 10 m.
#'
#' The output raster contains peat depth classes represented by integer
#' values from 0 to 6, where higher values correspond to greater peat depth.
#' Cells outside the Scotland land area boundary are assigned `NA`, and any
#' missing values within the land area are replaced with `0`.
#'
#' The output is written as an unsigned 8-bit integer raster with ZSTD
#' compression and tiled storage.
#'
#' @param source_path Character scalar. Path to the source raster file.
#' @param extent_list List defining the target spatial extent passed to
#' [standardise_ext()].
#' @param resolution Numeric scalar. Target raster resolution passed to
#' [standardise_res()].
#' @param land_area_path Character scalar. Path to a rasterised Scotland
#' land area mask used to constrain the output extent.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
process_robb_25_pd_std <- function(source_path, extent_list, resolution,
                               land_area_path) {
  
  r <- terra::rast(
    source_path
  ) |> 
    standardise_ext(extent_list) |> 
    discretise_peat_depth() |> 
    standardise_res(resolution, categorical = TRUE) |> 
    apply_land_area_mask(land_area_path)

  names(r) <- "peat_depth_class"
  
  output_path <- fs::path("data", "processed", "robb_25_pd_std.tif")
  
  terra::writeRaster(
    r,
    filename = output_path,
    datatype = "INT1U",
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process National Soil Map of Scotland dataset
#'
#' Extracts the National Soil Map of Scotland from a ZIP archive, reads the
#' source soil polygon layer, derives simplified peat depth classes from the
#' major soil subgroup description (`MSSG84_1`), rasterises the result to a
#' common grid, applies a Scotland land area mask, and writes the output to a
#' GeoTIFF.
#'
#' Soil mapping units containing the word `"peat"` are classified as peat soil
#' (depth class `6`), whilst units containing `"peaty"` are classified as
#' peaty soil (depth class `2`). All remaining soil classes are assigned
#' peat depth class `0`.
#'
#' The output raster contains peat depth classes represented by integer values
#' from 0 to 6, consistent with the classification scheme used elsewhere in
#' the project. Cells outside the Scotland land area boundary are assigned
#' `NA`, and missing values within the land area are replaced with `0`.
#'
#' The output is written as a tiled GeoTIFF with ZSTD compression.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to contain the National Soil
#'   Map source data.
#' @param extent_list List defining the target spatial extent passed to
#'   [extent_from_list()].
#' @param resolution Numeric scalar. Target raster resolution in map units
#'   (metres).
#' @param land_area_path Character scalar. Path to a rasterised Scotland
#'   land area mask used to constrain the output extent.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
process_nat_soil_map_std <- function(source_path, extent_list, resolution,
                                                                     land_area_path){
  
  extract_dir <- unzip_to_temp(source_path[[1]])
  
  v <- sf::st_read(
    fs::path(extract_dir,"Hutton_Soils_250K_v1.4", "qmsoils_UCSS_v1_3.shp")
  ) |> 
    select(MSSG84_1) |> 
    mutate(
      peat_depth_class = case_when( # order important
        str_detect(MSSG84_1, regex("\\bpeaty\\b", ignore_case = TRUE)) ~ 2L,
        str_detect(MSSG84_1, regex("\\bpeat\\b", ignore_case = TRUE)) ~ 6L,
        TRUE ~ 0L
      ),
      .keep = "unused"
    ) |> 
    terra::vect()
  
  r_template <- terra::rast(x = extent_from_list(extent_list),
                           resolution = resolution,
                           crs = "EPSG:27700")
  
  output <- terra::rasterize(v, r_template, field = "peat_depth_class", background = 0L) |> 
    apply_land_area_mask(land_area_path)
  
  names(output) <- "peat_depth_class"
  
  output_path <- fs::path("data", "processed", "nat_soil_map_std.tif")
  
  terra::writeRaster(
    output,
    filename = output_path,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process Scotland land area boundary - Mean High Water
#'
#' Extracts the Intermediate Zone 2022 boundary dataset, dissolves all
#' boundaries into a single land area polygon, attaches standard boundary
#' metadata, and writes the result to a GeoPackage.
#' 
#' The 'land area' is described here: https://www.spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/2978ed67-dade-42ec-b8e1-644e0b1f8cd8
#' This is the area to mean high water, excluding inland water bodies greater than 1 square kilometre in area.
#'
#' @param source_path Character vector or list containing the path to the
#' downloaded boundary ZIP file in the first element.
#'
#' @return Character scalar giving the path to the output GeoPackage.
#'
process_land_area_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "SG_IntermediateZoneBdry_2022_MHW.shp")
  )
  
  v <- terra::aggregate(v)

  v$boundary_key <- paste0("land_area",":::","mhw")
  
  v <- v[, "boundary_key"]
  
  output_path <- fs::path("data", "processed", "land_area.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}


#' Process local authority boundary dataset
#'
#' Extracts the local authority boundary dataset from a ZIP archive,
#' reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_key`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_las_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "pub_las.shp")
  )
  
  v$boundary_key <- paste0("local authority",":::",v$local_auth)
  
  v <- v[, "boundary_key"]
  
  output_path <- fs::path("data", "processed", "las.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Process Land Cover of Scotland 1988 dataset
#'
#' Extracts the Land Cover of Scotland 1988 (LCS88) dataset from a ZIP
#' archive, reads the source vector layer, assigns the British National
#' Grid coordinate reference system, and rasterises land cover classes
#' onto a template raster defined by the supplied extent and resolution.
#'
#' The resulting raster is written to a GeoTIFF for use in downstream
#' analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to be the archive containing
#'   the compressed dataset.
#' @param extent_list Named list defining the spatial extent of the output
#'   raster.
#' @param resolution Numeric. Output raster resolution in map units
#'   (metres).
#'
#' @return A character scalar giving the path to the processed GeoTIFF
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_lcs_88_std <- function(source_path, extent_list, resolution){
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "SG_LandCoverScotland_1988.shp")
  )
  
  terra::crs(v) <- "EPSG:27700"
  
  r_template <-terra::rast(x = extent_from_list(extent_list),
                           resolution = resolution)
  
  output <- terra::rasterize(v, r_template, field = "DOMTEXT",
                             background = 255) # all NAs classd as mapping offset
  
  lev <- terra::levels(output)[[1]] |> 
    rbind(list(255, "mapping offset"))
  
  levels(output) <- lev
  
  output_path <- fs::path("data", "processed", "lcs_88_std.tif")
  
  terra::writeRaster(
    output,
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
#' Process Land Capability for Agriculture (LCA) 1:250k dataset
#'
#' Extracts the LCA dataset from a ZIP
#' archive, reads the source vector layer, assigns the British National
#' Grid coordinate reference system, and rasterises broad capability categories
#' onto a template raster defined by the supplied extent and resolution.
#'
#' The resulting raster is written to a GeoTIFF for use in downstream
#' analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#' ZIP archive. The first element is assumed to be the archive containing
#' the compressed dataset.
#' @param extent_list Named list defining the spatial extent of the output
#' raster.
#' @param resolution Numeric. Output raster resolution in map units
#' (metres).
#'
#' @return A character scalar giving the path to the processed GeoTIFF
#' file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_lca_std <- function(source_path, extent_list, resolution){
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "LCA_250K.shp")
  )
  
  terra::crs(v) <- "EPSG:27700"
  
  r_template <-terra::rast(x = extent_from_list(extent_list),
                           resolution = resolution)
  
  output <- terra::rasterize(v, r_template, field = "SqMid")
  
  output_path <- fs::path("data", "processed", "lca_std.tif")
  
  terra::writeRaster(
    output,
    filename = output_path,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
  
} 

#' Process GHGI condition dataset
#'
#' Reads the source vector dataset, assigns the British National Grid
#' coordinate reference system, and rasterises the `Condition` attribute
#' onto a template raster defined by the supplied extent and resolution.
#'
#' The resulting raster is written to a GeoTIFF for use in downstream
#' analyses.
#'
#' @param source_path Character scalar. Path to the source vector dataset.
#' @param extent_list Named list defining the spatial extent of the output
#'   raster.
#' @param resolution Numeric. Output raster resolution in map units
#'   (metres).
#'
#' @return A character scalar giving the path to the processed GeoTIFF
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_ghgi_condition_std <- function(source_path, extent_list, resolution){
  
  v <- terra::vect(source_path)
  
  terra::crs(v) <- "EPSG:27700"
  
  r_template <-terra::rast(x = extent_from_list(extent_list),
                           resolution = resolution,
                           crs = "EPSG:27700")
  
  output <- terra::rasterize(v, r_template, field = "Condition")
  
  output_path <- fs::path("data", "processed", "ghgi_condition_std.tif")
  
  terra::writeRaster(
    output,
    filename = output_path,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process the GHGI peatland extent dataset
#'
#' Reads the source vector dataset, assigns the British National Grid
#' coordinate reference system, rasterises peatland extent onto a template
#' raster defined by the supplied extent and resolution, applies a Scotland
#' land area mask, and writes the result to a GeoTIFF.
#'
#' The output raster is a binary peatland extent layer. Cells whose centres
#' fall within the source peatland geometry are assigned peat depth class
#' `6`, corresponding to the deepest peat class used elsewhere in the
#' project. All other cells within the land area are assigned `0`.
#'
#' Cells outside the Scotland land area boundary are assigned `NA`, and any
#' missing values within the land area are replaced with `0`.
#'
#' The output is written as an unsigned 8-bit integer raster with ZSTD
#' compression and tiled storage.
#'
#' @param source_path Character scalar. Path to the source vector dataset.
#' @param extent_list List defining the spatial extent of the output raster.
#' @param resolution Numeric scalar. Output raster resolution in map units
#' (metres).
#' @param land_area_path Character scalar. Path to a rasterised Scotland
#' land area mask used to constrain the output extent.
#'
#' @return A character scalar giving the path to the processed raster file.
#' Intended for use with `targets` file targets (`format = "file"`).
process_ghgi_extent_std <- function(source_path, extent_list, resolution,
                                    land_area_path){
  
  v <- terra::vect(source_path)
  v$peat_depth_class <- 6L
  
  terra::crs(v) <- "EPSG:27700"
  
  r_template <-terra::rast(x = extent_from_list(extent_list),
                           resolution = resolution,
                           crs = "EPSG:27700")
  
  output <- terra::rasterize(v, r_template, field = "peat_depth_class", background = 0L) |> 
    apply_land_area_mask(land_area_path)
  
  names(output) <- "peat_depth_class"
  
  output_path <- fs::path("data", "processed", "ghgi_extent_std.tif")
  
  terra::writeRaster(
    output,
    filename = output_path,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Rasterise a boundary dataset
#'
#' Reads a processed boundary GeoPackage, creates a raster template from the
#' supplied extent and resolution, and rasterises the `boundary_key`
#' attribute onto the target grid.
#'
#' The output raster is written as an unsigned 16-bit integer GeoTIFF with
#' ZSTD compression and tiled storage. The output filename is derived from the
#' input GeoPackage path by replacing the `.gpkg` suffix with `_rast.tif`.
#'
#' @param source_path Character scalar. Path to a processed boundary
#' GeoPackage containing a `boundary_key` field.
#' @param extent_list List defining the spatial extent of the output raster.
#' @param resolution Numeric scalar. Output raster resolution in map units
#' (metres).
#'
#' @return A character scalar giving the path to the rasterised boundary
#' GeoTIFF. Intended for use with `targets` file targets
#' (`format = "file"`).
#'
rasterize_boundary <- function(source_path, extent_list, resolution){
  
  v <- terra::vect(source_path)
  
  terra::crs(v) <- "EPSG:27700"
  
  r_template <-terra::rast(x = extent_from_list(extent_list),
                           resolution = resolution,
                           crs = "EPSG:27700")
  
  output <- terra::rasterize(v, r_template, field = "boundary_key")
  
  output_path <- stringr::str_replace(source_path, "\\.gpkg", "_rast.tif")
  
  terra::writeRaster(
    output,
    filename = output_path,
    overwrite = TRUE,
    datatype = "INT2U",
    gdal = c(
      "COMPRESS=ZSTD",
      "TILED=YES"
    )
  )
  
  output_path
}

#' Process Loch Lomond and the Trossachs National Park boundary
#'
#' Extracts the boundary dataset from a ZIP archive,
#' reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_key`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_lltnp_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "SG_LochLomondTrossachsNationalPark_2002.shp")
  )
  
  v$boundary_key <- paste0("national park", ":::", "lltnp")
  
  v <- v[, "boundary_key"]
  
  output_path <- fs::path("data", "processed", "lltnp.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Process Cairngorms National Park boundary
#'
#' Extracts the boundary dataset from a ZIP archive,
#' reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_class` and `boundary_name`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path to the downloaded
#'   ZIP archive. The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_cnp_bdry <- function(source_path){
  
  zip_file_path <- source_path[[1]]
  
  extract_dir <- unzip_to_temp(zip_file_path)
  
  v <- terra::vect(
    fs::path(extract_dir, "SG_CairngormsNationalPark_2010.shp")
  )
  
  v$boundary_key <- paste0("national park",":::","cnp")
  
  v <- v[, "boundary_key"]
  
  output_path <- fs::path("data", "processed", "cnp.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Process main river and coastal catchment boundaries
#'
#' Reads the SEPA main river and coastal catchment dataset, retains
#' catchments with an area of at least 100 km² after cropping to the supplied
#'  land area, and standardises identifiers by creating a
#' `boundary_key` field. The processed boundaries are written to a
#' GeoPackage for use in downstream analyses.
#'
#' @param source_path Character scalar giving the path or URL of the source
#' catchment dataset.
#' @param land_area An `sf` polygon object defining the area to which the
#' catchment boundaries should be cropped.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#' file. Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' The output contains only main river and coastal catchments in Scotland with
#' a surface area of at least 100 km^2.
#' A unique boundary identifier is created in the form:
#'
#' `catchment:::<catchment_number> - <catchment_name>`
#'
#' where `<catchment_number>` is the SEPA catchment identifier and
#' `<catchment_name>` is the published catchment name.
#'
process_catchments_bdry <- function(source_path, land_area){

  sf::sf_use_s2(FALSE)
   
  v <- sf::st_read(source_path[[1]]) |> 
    mutate(boundary_key = paste0("catchment",":::",gi02_catchno," - ",catchment_name)) |> 
    sf::st_crop(land_area) |> 
    filter(as.numeric(st_area(geom)) >= (100*1000*1000)) |> # 100 km2
    select(boundary_key) 

  output_path <- fs::path("data", "processed", "catchments.gpkg")

  sf::write_sf(v,
               output_path,
               delete_layer = TRUE)

  output_path
}

#' Process agricultural land boundary - (BPS claimed land and common grazings)
#'
#' Reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_key`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path source file
#' The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_agri_bdry <- function(source_path){
  
  v <- terra::vect(source_path)
  
  v$boundary_key <- paste0("agricultural land",":::",v$class)
  
  v <- v[, "boundary_key"]
  
  output_path <- fs::path("data", "processed", "agri.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Process public land boundary
#'
#' Reads the boundary geometries, standardises the boundary attributes to
#' a common schema (`boundary_key`), and writes the
#' result to a GeoPackage for use in downstream analyses.
#'
#' @param source_path Character vector containing the path source file
#' The first element is assumed to be the archive containing
#'   the boundary dataset.
#'
#' @return A character scalar giving the path to the processed GeoPackage
#'   file. Intended for use with `targets` file targets (`format = "file"`).
#'
process_public_land_bdry <- function(source_path){
  
  v <- terra::vect(source_path)
  
  v$boundary_key <- paste0("public land",":::","public land")
  
  v <- v[, "boundary_key"]
  
  output_path <- fs::path("data", "processed", "public_land.gpkg")
  
  terra::writeVector(
    v,
    filename = output_path,
    overwrite = TRUE
  )
  
  output_path
}

#' Create a hexagonal boundary grid (helper function)
#'
#' Creates a hexagonal tessellation covering a supplied land area polygon,
#' clips cells intersecting the land boundary, assigns unique boundary
#' identifiers, and writes the resulting grid to a GeoPackage.
#'
#' Hexagons wholly contained within the land area are retained unchanged,
#' whilst only boundary-intersecting cells are clipped. This reduces the
#' number of computationally expensive intersection operations required.
#'
#' Each output feature is assigned a unique `boundary_key` in the format:
#'
#' `"<output_name>:::<id>"`
#'
#' where `<id>` is a sequential identifier.
#'
#' @param land_area An `sf` polygon object defining the area to be covered
#'   by the grid.
#' @param cell_size Numeric scalar giving the hexagon cell size in map units
#'   (metres), passed to [sf::st_make_grid()].
#' @param output_name Character scalar used to construct the output filename
#'   and `boundary_key` values.
#'
#' @return A character scalar giving the path to the output GeoPackage file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' The output GeoPackage is written to `data/processed/` and contains a
#' single attribute, `boundary_key`, together with the hexagonal grid
#' geometry. The coordinate reference system is inherited from
#' `land_area`.
make_hex_grid <- function(land_area,
                          cell_size,
                          output_name){
  
  sf::sf_use_s2(FALSE)
  
  hex_grid <- sf::st_make_grid(
    land_area,
    square = FALSE,
    cellsize = cell_size,
  ) |>
    sf::st_sf()
  
  # Hexagons completely within land area
  inside <- lengths(
    sf::st_within(hex_grid, land_area)
  ) > 0
  
  hex_inside <- hex_grid[inside, ]
  
  # Only these need clipping
  hex_boundary <- hex_grid[!inside, ]
  
  hex_boundary <- sf::st_intersection(
    hex_boundary,
    land_area
  )
  
  hex_grid_masked <- dplyr::bind_rows(
    hex_inside,
    hex_boundary
  )
  
  hex_grid_masked$boundary_key <- paste0(
    output_name,
    ":::",
    seq_len(nrow(hex_grid_masked)))
  
  output_path <- fs::path(
    "data",
    "processed",
    paste0(output_name, ".gpkg")
  )
  
  sf::write_sf(
    hex_grid_masked,
    output_path,
    delete_dsn = TRUE
  )
  
  output_path
}

process_hex_grid_10km <- function(land_area){
  make_hex_grid(land_area,
                10000,
                "hex_grid_10km")
}

process_hex_grid_5km <- function(land_area){
  make_hex_grid(land_area,
                5000,
                "hex_grid_5km")
}
 


pa_processing_helper <- function(source_path, output_filename){
  
  output_path <- fs::path(
    "data",
    "processed",
    output_filename
  )
  
  sf::st_read(source_path) |> 
    st_make_valid() |> 
    select(GRANT_ID) |> 
    rename("grant_id" = "GRANT_ID") |> 
    group_by(grant_id) |> 
    summarise(
      geom = sf::st_union(geom)
    ) |> 
    ungroup() |> 
    sf::write_sf(output_path,
                 delete_dsn = TRUE)
  
  output_path
}

process_pa_centroids_std <- function(source_path){
  pa_processing_helper(source_path[[1]], "pa_centroids_std.gpkg")
}

process_pa_footprints_std <- function(source_path){
  pa_processing_helper(source_path[[1]], "pa_footprints_std.gpkg")
}

#' Process Peatland ACTION GHGI 2024 restoration dataset
#'
#' Reads the spatial component of the Peatland ACTION 2024 greenhouse gas
#' inventory (GHGI) submission dataset, standardises key attributes, crops
#' restoration footprints to the Scotland land area boundary, and writes the
#' result to a GeoPackage for downstream analysis.
#'
#' Original restoration areas are calculated prior to cropping. A correction
#' factor is then derived as the ratio of the original area to the cropped
#' area, allowing subsequent analyses to preserve reported restoration areas
#' when allocating them to spatial units.
#'
#' Records with restoration year 2025 or later are excluded, as is grant
#' `500764`, which is treated as an exception and omitted from the dataset.
#'
#' The output contains the following variables:
#'
#' * `site_id` - Unique restoration site identifier.
#' * `source` - Data source description.
#' * `year` - Restoration year.
#' * `area_ha` - Original restoration area in hectares.
#' * `area_correction_factor` - Adjustment factor accounting for cropping to
#'   the Scotland land area boundary.
#'
#' @param source_path Character scalar. Path to the input spatial dataset.
#' @param land_area An `sf` polygon object defining the Scotland land area
#'   boundary used for cropping.
#'
#' @return A character scalar giving the path to the processed GeoPackage file.
#' Intended for use with `targets` file targets (`format = "file"`).
process_pa_ghgi_2024_std <- function(source_path, land_area){
  output_path <- fs::path(
    "data",
    "processed",
    "pa_ghgi_2024_std.gpkg"
  )
  
  sf::st_read(source_path) |> 
    rename(year = financial_year_end) |> 
    filter(year < 2025,
           grant_id != "500764") |> 
    st_make_valid() |> 
    mutate(area_ha = (st_area(geom) |> as.numeric()) / 10000) |> 
    sf::st_crop(land_area) |> 
    mutate(cropped_area_ha = (st_area(geom) |> as.numeric()) / 10000,
           area_correction_factor = area_ha / cropped_area_ha,
           source = "Peatland ACTION GHGI submission 2024") |> 
    rename(site_id = grant_id) |> 
    select(site_id, source, year, area_ha, area_correction_factor) |> 
    sf::write_sf(output_path,
                 delete_dsn = TRUE)
  
  output_path
}

#' Process Evans et al. (2017) peatland restoration dataset
#'
#' Reads the Evans et al. (2017) restoration dataset, extracts restoration
#' site identifiers, reported restoration areas, and British National Grid
#' coordinates, converts the coordinates to point geometries, and generates
#' estimated restoration footprints using [create_footprint()].
#'
#' The source dataset contains site locations and restoration areas but does
#' not provide restoration footprint polygons. Footprints are therefore
#' approximated by buffering each site location to match the reported
#' restoration area and clipping the resulting geometry to the Scotland land
#' area boundary.
#'
#' Because restoration dates are not available in the source dataset, an
#' arbitrary placeholder year of 2000 is assigned to all records to enable
#' integration with other restoration datasets.
#'
#' The output contains the following variables:
#'
#' * `site_id` - Unique restoration site identifier.
#' * `source` - Data source description.
#' * `year` - Restoration year (assigned as 2000).
#' * `area_ha` - Reported restoration area in hectares.
#'
#' @param source_path Character scalar. Path to the input Excel workbook.
#' @param land_area An `sf` polygon object defining the Scotland land area
#'   boundary used when generating restoration footprints.
#'
#' @return A character scalar giving the path to the processed GeoPackage file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' Records without valid British National Grid coordinates are excluded.
#' Restoration footprints are not present in the source data and are inferred
#' solely from the reported restoration area and site location.
process_evans_2017_std <- function(source_path, land_area){
  
  output_path <- fs::path(
    "data",
    "processed",
    "evans_2017.gpkg"
  )
  
  data <- openxlsx::read.xlsx(source_path, sep.names = "_") |> 
    select(`Project_Area_(ha)`,
           `BNG_Easting_/_Northing`,
           ID) |> 
    rename(area_ha = `Project_Area_(ha)`,
           bng_en = `BNG_Easting_/_Northing`,
           site_id = ID) |> 
    filter(!is.na(bng_en)) |> 
    separate(
      bng_en,
      into = c("Easting", "Northing"),
      sep = ","
    ) |> 
    mutate(
      Easting = as.numeric(trimws(Easting)),
      Northing = as.numeric(trimws(Northing))
    ) |> 
    st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |> 
    rowwise() |> 
    mutate(geometry = create_footprint(
      geom = geometry,
      target_ha = area_ha,
      clip_geom = land_area
    ),
    year = 2000, # arbitrary 
    source = "Evans et al., 2017") |> 
    select(site_id, source, year, area_ha) |> 
    sf::st_as_sf() |> 
    sf::write_sf(output_path,
                 delete_dsn = TRUE)
  
  output_path
  
}

#' Process UKCEH peat extraction restoration dataset
#'
#' Reads the UK Centre for Ecology & Hydrology (UKCEH) peat extraction
#' restoration dataset, validates geometries, calculates restoration areas,
#' crops restoration footprints to the Scotland land area boundary, and writes
#' the result to a GeoPackage for downstream analysis.
#'
#' Original restoration areas are calculated prior to cropping. A correction
#' factor is then derived as the ratio of the original area to the cropped
#' area, allowing subsequent analyses to preserve reported restoration areas
#' when allocating them to spatial units.
#'
#' Because restoration dates are not available in the source dataset, an
#' arbitrary placeholder year of 2000 is assigned to all records to enable
#' integration with other restoration datasets.
#'
#' The output contains the following variables:
#'
#' * `site_id` - Site identifier derived from the source `Name` field.
#' * `source` - Data source description.
#' * `year` - Restoration year (assigned as 2000).
#' * `area_ha` - Original restoration area in hectares.
#' * `area_correction_factor` - Adjustment factor accounting for cropping to
#'   the Scotland land area boundary.
#'
#' @param source_path Character scalar. Path to the input spatial dataset.
#' @param land_area An `sf` polygon object defining the Scotland land area
#'   boundary used for cropping.
#'
#' @return A character scalar giving the path to the processed GeoPackage file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' The source dataset contains restoration footprint geometries but does not
#' provide restoration dates. A placeholder year of 2000 is therefore assigned
#' to all records. Cropping may reduce the mapped footprint area where features
#' extend beyond the Scotland land area boundary; the
#' `area_correction_factor` can be used to account for this in downstream
#' analyses.
process_ukceh_extr_rest_std <- function(source_path, land_area){
  output_path <- fs::path(
    "data",
    "processed",
    "ukceh_extr_rest_std.gpkg"
  )
  
  sf::st_read(source_path) |> 
    st_make_valid() |> 
    mutate(area_ha = (st_area(geom) |> as.numeric()) / 10000) |> 
    sf::st_crop(land_area) |> 
    mutate(cropped_area_ha = (st_area(geom) |> as.numeric()) / 10000,
           area_correction_factor = area_ha / cropped_area_ha,
           source = "UKCEH Peat Extraction Database",
           year = 2000) |> # arbitrary 
    rename("site_id" = Name) |> 
    select(site_id, source, year, area_ha, area_correction_factor) |> 
    sf::write_sf(output_path,
                 delete_dsn = TRUE)
  
  output_path
  
}

# write outputs ####

#' Write output datasets to disk
#'
#' Saves the final analytical outputs produced by the pipeline as RDS files in
#' the `data/outputs/` directory and returns the paths to the written files.
#'
#' The function creates the output directory if it does not already exist and
#' writes each dataset using [saveRDS()]. The returned file paths can be used
#' as a `targets` file target to track the output artefacts.
#'
#' The following datasets are written:
#'
#' * `rewetting_summary_dataset.rds`
#' * `restoration_summary_dataset.rds`
#' * `baseline_condition_summary_dataset.rds`
#' * `simplified_condition_time_series_dataset.rds`
#'
#' @param rewetting_summary_dataset Data frame containing summaries of
#'   peatland rewetting activity.
#' @param restoration_summary_dataset Data frame containing summaries of
#'   peatland restoration activity.
#' @param baseline_condition_summary_dataset Data frame containing summaries of
#'   baseline peat condition.
#' @param simplified_condition_time_series_dataset Data frame containing the
#'   simplified peat condition time series.
#'
#' @return A character vector containing the paths to the output RDS files.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' Output files are written to the `data/outputs/` directory. Existing files
#' with the same names are overwritten.
write_output_datasets <- function(
    rewetting_summary_dataset,
    restoration_summary_dataset,
    baseline_condition_summary_dataset,
    simplified_condition_time_series_dataset
) {
  
  output_dir <- fs::path("data", "outputs")
  
  fs::dir_create(output_dir)
  
  rewetting_file <- fs::path(
    output_dir,
    "rewetting_summary_dataset.rds"
  )
  
  restoration_file <- fs::path(
    output_dir,
    "restoration_summary_dataset.rds"
  )
  
  baseline_file <- fs::path(
    output_dir,
    "baseline_condition_summary_dataset.rds"
  )
  
  timeseries_file <- fs::path(
    output_dir,
    "simplified_condition_time_series_dataset.rds"
  )
  
  saveRDS(rewetting_summary_dataset, rewetting_file)
  saveRDS(restoration_summary_dataset, restoration_file)
  saveRDS(baseline_condition_summary_dataset, baseline_file)
  saveRDS(simplified_condition_time_series_dataset, timeseries_file)
  
  c(
    rewetting_file,
    restoration_file,
    baseline_file,
    timeseries_file
  )
}

