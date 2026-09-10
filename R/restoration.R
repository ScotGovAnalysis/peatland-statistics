#' Categorise Peatland ACTION records by spatial data availability
#'
#' Combines non-spatial Peatland ACTION restoration records with available
#' footprint and centroid datasets to determine the level of spatial
#' information available for each grant.
#'
#' Records are classified into one of three spatial data categories:
#'
#' * `"footprint"` - a restoration footprint polygon is available.
#' * `"centroids only"` - a site centroid is available but no footprint.
#' * `"none"` - no spatial representation is available.
#'
#' The function also aggregates restoration area by grant and financial year,
#' calculates the total restored area associated with each grant, and derives
#' the proportion of each grant's restoration delivered in each year.
#'
#' To improve matching between datasets, grant identifiers of the form
#' `"5xxxxx[a-z]"` are harmonised where the corresponding base identifier
#' exists in the centroid dataset (for example, `"500963a"` is matched to
#' `"500963"`).
#'
#' The output contains the following additional variables:
#'
#' * `spat_data_class` - Spatial data availability category.
#' * `total_ha_restored` - Total restored area associated with the grant.
#' * `frac_restored_in_year` - Proportion of the total restoration area
#'   delivered in the given financial year.
#'
#' @param pa_non_spatial_path Character scalar. Path to the non-spatial
#'   Peatland ACTION dataset stored as an RDS file.
#' @param pa_footprints_path Character scalar. Path to the Peatland ACTION
#'   footprint dataset.
#' @param pa_centroids_path Character scalar. Path to the Peatland ACTION
#'   centroid dataset.
#'
#' @return A tibble containing non-spatial restoration records enriched with
#' spatial data availability classifications and derived area metrics.
#'
#' @details
#' Restoration area is aggregated to the grant-year level before spatial data
#' availability is assigned. The resulting dataset forms the basis for
#' subsequent creation of spatial restoration datasets and temporal allocation
#' of restoration activity.
categorise_pa_by_spatial_data_availability <- function(
    pa_non_spatial_path,
    pa_footprints_path,
    pa_centroids_path){
  
  pa_footprints_keys <- sf::read_sf(pa_footprints_path) |> 
    sf::st_drop_geometry() |> 
    select(grant_id) |> 
    mutate(footprint = TRUE)
  
  pa_centroids_keys <- sf::read_sf(pa_centroids_path) |> 
    sf::st_drop_geometry() |> 
    select(grant_id) |> 
    mutate(centroids = TRUE)
  
  # correct missed matches of form 5-----[a-z], e.g. 500963a and 500963
  valid_base_ids <- pa_centroids_keys$grant_id
  
  non_spatial <- readRDS(pa_non_spatial_path) |> 
    mutate(
      grant_id = case_when(
        str_detect(grant_id, "^5\\d{5}[A-Za-z]$") &
          str_remove(grant_id, "[A-Za-z]$") %in% valid_base_ids ~
          str_remove(grant_id, "[A-Za-z]$"),
        TRUE ~ grant_id
      )
    ) |> 
    group_by(grant_id, financial_year_end) |> 
    summarise(hectares = sum(hectares, na.rm = TRUE),
              delivery_partner = first(delivery_partner),
              .groups = "drop")
  
  # join and process
  non_spatial |> 
    left_join(pa_footprints_keys) |> 
    left_join(pa_centroids_keys) |> 
    mutate(spat_data_class = case_when(
             footprint ~ "footprint",
             centroids ~ "centroids only",
             TRUE ~ "none")
           ) |> 
    group_by(grant_id) |> 
    mutate(total_ha_restored = sum(hectares)) |> 
    ungroup() |> 
    mutate(frac_restored_in_year = hectares / total_ha_restored) |> 
    select(-footprint, -centroids)
    
}

#' Create an area-constrained footprint from point geometry
#'
#' Creates a footprint polygon around one or more point locations by buffering
#' the input geometry to achieve a target area. Where the resulting footprint
#' extends beyond a clipping geometry, the footprint is cropped and the buffer
#' radius is iteratively adjusted so that the final clipped footprint matches
#' the requested area as closely as possible.
#'
#' The function is intended for situations where only point locations and a
#' target area are available. This is used, for example, to create approximate
#' restoration footprints from site centroids and reported restoration areas.
#'
#' For multipoint geometries, the target area is distributed across all points
#' by applying a common buffer radius to each component point.
#'
#' @param geom An `sf` geometry object containing one or more point
#'   geometries.
#' @param target_ha Numeric scalar giving the desired footprint area in
#'   hectares.
#' @param clip_geom An `sf` polygon object defining the area within which the
#'   footprint must be contained.
#'
#' @return An `sf` geometry object representing the generated footprint
#' polygon.
#'
#' @details
#' The footprint area is matched to the requested area using
#' [stats::uniroot()] to identify the buffer radius that minimises the
#' difference between the footprint area and the target area. Where clipping
#' is required, area calculations are performed on the clipped geometry rather
#' than the unconstrained buffer.
#'
#' To improve performance, clipping operations are restricted to a subset of
#' the clipping geometry intersecting a bounding box around the maximum
#' possible buffer extent.

create_footprint <- function(
    geom,
    target_ha,
    clip_geom
) {
  
  target_m2 <- target_ha * 10000
  
  n_pts <- length(sf::st_cast(geom, "POINT"))
  
  r_start <- sqrt(target_m2 / (n_pts * pi))
  
  upper_radius <- r_start * 10
  
  max_buffer <- sf::st_buffer(geom, upper_radius)
  
  local_land <- sf::st_crop(
    sf::st_geometry(clip_geom),
    sf::st_bbox(
      max_buffer
    )
  )
  
  needs_clipping <- !all(
    sf::st_within(
      max_buffer,
      local_land,
      sparse = FALSE
    )
  )
  
  area_difference <- function(radius) {
    
    buffer <- sf::st_buffer(geom, radius)
    
    area <- if (needs_clipping) {
      sf::st_intersection(
        buffer,
        local_land
      ) |>
        sf::st_area() |>
        sum() |>
        as.numeric()
    } else {
      sf::st_area(buffer) |>
        sum() |>
        as.numeric()
    }
    
    area - target_m2
  }
  
  radius <- uniroot(
    area_difference,
    interval = c(r_start, upper_radius)
  )$root
  
  buffer <- sf::st_buffer(geom, radius)
  
  footprint <- if (needs_clipping) {
    sf::st_intersection(
      buffer,
      local_land
    )
  } else {
    buffer
  }
  
  footprint
}

#' Format and quality-assure Peatland ACTION footprint data
#'
#' Reads Peatland ACTION footprint geometries, joins them to restoration
#' records classified as having footprint-based spatial data, derives quality
#' assurance metrics, assesses overlap between footprint polygons, crops
#' footprints to the Scotland land area boundary, and calculates area
#' correction factors for downstream analyses.
#'
#' The function compares the mapped footprint area with the reported restored
#' area for each grant and flags records where the two measures are deemed to
#' agree. Agreement is defined as either:
#'
#' * An absolute area difference of no more than 1 hectare; or
#' * A relative area difference of no more than 2%.
#'
#' The function also identifies overlapping footprints and records whether at
#' least 5% of a footprint's area is covered by another restoration footprint.
#'
#' After quality assurance checks, footprints are cropped to the Scotland land
#' area boundary and an area correction factor is calculated to preserve the
#' reported restoration area in downstream spatial summaries.
#'
#' The output includes the following derived variables:
#'
#' * `geom_ha` - Area of the input footprint geometry in hectares.
#' * `area_diff_ha` - Difference between mapped and reported restoration area.
#' * `area_diff_pct` - Relative difference between mapped and reported area.
#' * `qa_area_match` - Indicator of acceptable agreement between mapped and
#'   reported area.
#' * `max_overlap_ha` - Maximum overlap area with another footprint.
#' * `prop_covered` - Proportion of footprint area overlapped by another
#'   footprint.
#' * `overlap_5_perc` - Indicator for footprints with at least 5% overlap.
#' * `cropped_area_ha` - Footprint area after cropping to land area.
#' * `area_correction_factor` - Ratio of reported restoration area to cropped
#'   footprint area.
#'
#' @param spat_data_avail_df Tibble produced by
#'   [categorise_pa_by_spatial_data_availability()] containing restoration
#'   records and spatial data classifications.
#' @param pa_footprints_path Character scalar. Path to the Peatland ACTION
#'   footprint dataset.
#' @param land_area An `sf` polygon object defining the Scotland land area
#'   boundary used for cropping.
#'
#' @return An `sf` object containing footprint geometries together with
#' restoration attributes and quality-assurance metrics.
#'
#' @details
#' Footprint overlap statistics are intended as data quality indicators and do
#' not alter the geometries themselves. The resulting dataset is subsequently
#' combined with centroid-derived and non-spatial restoration records to create
#' a unified restoration dataset for analysis.
format_footprints <- function(
  spat_data_avail_df,
  pa_footprints_path,
  land_area){
  
  footprints <-
    sf::read_sf(pa_footprints_path) |> 
    right_join(spat_data_avail_df |> 
                 filter(spat_data_class == "footprint")) |> 
    mutate(
      geom_ha = as.numeric(st_area(geom)) / 10000,
      area_diff_ha = geom_ha - total_ha_restored,
      area_diff_pct = abs(area_diff_ha) / total_ha_restored,
      qa_area_match = abs(area_diff_ha) <= 1 | area_diff_pct <= 0.02
    )
  
  overlaps <- st_intersection(
    footprints |> select(grant_id, geom_ha),
    footprints |> select(grant_id, geom_ha)
  ) |> 
    filter(grant_id != grant_id.1) |> 
    mutate(overlap_ha = st_area(geom) |>
             as.numeric() |>
             (\(x) x / 10000)()) |> 
    st_drop_geometry() |> 
    group_by(grant_id) |> 
    summarise(max_overlap_ha = max(overlap_ha)) |> 
    ungroup()
  
  footprints <- left_join(footprints,
                          overlaps) |> 
    mutate(prop_covered = max_overlap_ha / geom_ha,
           overlap_5_perc = if_else(prop_covered >= 0.05, TRUE, FALSE)) |> 
    st_crop(land_area) |> 
    mutate(cropped_area_ha = st_area(geom) |> as.numeric() |> (\(x) x / 10000)(),
           area_correction_factor = total_ha_restored / cropped_area_ha)
  
  footprints
  
}

#' Create a combined Peatland ACTION restoration dataset
#'
#' Combines all Peatland ACTION restoration records into a single dataset,
#' incorporating footprint-based records, centroid-derived footprints, and
#' records with no spatial representation. The resulting dataset provides a
#' unified source for downstream restoration analyses.
#'
#' Records derived from centroids are assigned an
#' `area_correction_factor` of 1 because their generated footprints are
#' constructed to match the reported restoration area. Records without
#' spatial data are retained with missing geometry and are included so that
#' non-spatial restoration activity can be represented in aggregate summaries.
#'
#' Missing values in key allocation variables are replaced with defaults:
#'
#' * `frac_restored_in_year` defaults to 0.
#' * `area_correction_factor` defaults to 1.
#'
#' The combined dataset is written to a GeoPackage for use in subsequent
#' spatial and temporal restoration analyses.
#'
#' @param spat_data_avail_df Tibble produced by
#'   [categorise_pa_by_spatial_data_availability()] containing restoration
#'   records and spatial data classifications.
#' @param pa_footprints An `sf` object containing validated restoration
#'   footprint geometries and associated attributes.
#' @param pa_centroids An `sf` object containing footprints generated from
#'   restoration centroids and associated attributes.
#'
#' @return A character scalar giving the path to the output GeoPackage file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' The output contains three classes of restoration record:
#'
#' * Grants with mapped restoration footprints.
#' * Grants represented by centroid-derived footprints.
#' * Grants with no available spatial representation.
#'
#' Together, these provide the complete Peatland ACTION restoration dataset
#' used throughout the analytical pipeline.
create_combined_pa_dataset <- function(spat_data_avail_df,
                                       pa_footprints, 
                                       pa_centroids){
  
  output <- pa_footprints |> 
    bind_rows(pa_centroids |> mutate(area_correction_factor = 1)) |> 
    bind_rows(spat_data_avail_df |> filter(spat_data_class == "none")) |> 
    mutate(frac_restored_in_year = frac_restored_in_year |> replace_na(0),
           area_correction_factor = area_correction_factor |> replace_na(1))
  
  output_path <-fs::path(
    "data",
    "processed",
    "pa_restored_dataset.gpkg")
  
  sf::write_sf(
    output,
    output_path,
    delete_dsn = TRUE
  )
  
  output_path
  
}

#' Combine rewetting datasets into a unified spatial dataset
#'
#' Reads multiple processed rewetting datasets, harmonises key fields,
#' combines them into a single spatial dataset, and writes the result to a
#' GeoPackage for downstream analysis.
#'
#' The function ensures that site identifiers are stored consistently as
#' character values across all input datasets before concatenation. Missing
#' values of `area_correction_factor` are replaced with `1`, indicating that
#' no area adjustment is required.
#'
#' The resulting dataset provides a single source of rewetting activity from
#' multiple input datasets and is used in subsequent spatial summarisation and
#' condition time series analyses.
#'
#' @param input_paths Character vector of file paths to processed rewetting
#'   datasets. Each dataset must be readable by [sf::st_read()] and conform to
#'   the expected schema.
#'
#' @return A character scalar giving the path to the output GeoPackage file.
#' Intended for use with `targets` file targets (`format = "file"`).
#'
#' @details
#' All input datasets are combined using [dplyr::bind_rows()]. The output is
#' written to `data/processed/combined_rewetting_dataset.gpkg` and serves as
#' the canonical rewetting dataset for downstream analyses.
create_combined_rewetting_dataset <- function(input_paths) {
  
  output_path <- fs::path(
    "data",
    "processed",
    "combined_rewetting_dataset.gpkg"
  )
  
  data <- purrr::map(
    input_paths,
    \(x) {
      sf::st_read(x, quiet = TRUE) |>
        mutate(
          site_id = as.character(site_id)
        )
    }
  ) |>
    dplyr::bind_rows() |> 
    mutate(area_correction_factor = replace_na(area_correction_factor, 1))
  
  sf::write_sf(data, output_path, delete_dsn = TRUE)
  
  output_path
}

#' Summarise rewetting activity by boundary geography
#'
#' Intersects rewetting polygons with a boundary dataset and calculates the
#' area of rewetting occurring within each boundary unit. Rewetting areas are
#' adjusted using any supplied area correction factors and aggregated by
#' boundary, source dataset, and year.
#'
#' To improve performance, rewetting features that do not intersect the
#' boundary dataset are removed prior to the spatial intersection operation.
#'
#' The output contains one row per boundary, source, and year combination,
#' together with the total rewetting area allocated to that boundary.
#'
#' @param rewetting An `sf` object containing rewetting geometries and the
#'   variables `source`, `year`, and `area_correction_factor`.
#' @param boundary_path Character scalar. Path to a boundary dataset readable
#'   by [sf::st_read()]. The dataset must contain a `boundary_key` field in
#'   the format `"boundary_class:::boundary_name"`.
#'
#' @return A tibble containing:
#'
#' * `boundary_key` - Unique boundary identifier.
#' * `boundary_class` - Boundary geography type.
#' * `boundary_name` - Boundary name.
#' * `source` - Rewetting data source.
#' * `year` - Year associated with the rewetting activity.
#' * `area_ha` - Total rewetting area (hectares) within the boundary.
#'
#' @details
#' Areas are calculated from the intersected geometries and multiplied by
#' `area_correction_factor` to account for any adjustments applied during
#' preprocessing. The function uses planar geometry operations
#' (`sf_use_s2(FALSE)`) to ensure compatibility with the British National Grid
#' projection used throughout the pipeline.
summarise_rewetting_by_boundary <- function(
    rewetting,
    boundary_path
){
  
  sf::sf_use_s2(FALSE)
  
  boundary <- sf::st_read(boundary_path, quiet = TRUE) |>
    dplyr::select(boundary_key)
  
  rewetting <- rewetting |>
    dplyr::select(source, year, area_correction_factor)
  
  hits <- sf::st_intersects(
    rewetting,
    boundary
  )
  
  rewetting <- rewetting[
    lengths(hits) > 0,
  ]
  
  sf::st_intersection(
    rewetting,
    boundary
  ) |>
    dplyr::mutate(
      area_ha =
        as.numeric(sf::st_area(geom)) / 10000 * area_correction_factor
    ) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(
      boundary_key,
      source,
      year
    ) |>
    dplyr::summarise(
      area_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    tidyr::separate_wider_delim(
      boundary_key,
      delim = ":::",
      names = c("boundary_class", "boundary_name"),
      cols_remove = FALSE
    )
}

#' Summarise rewetting activity for the Scotland land area
#'
#' Produces national-level summaries of rewetting activity by combining
#' spatially explicit rewetting datasets with additional non-spatial
#' rewetting records and aggregating areas by source and year.
#'
#' Rewetting areas are adjusted using any available
#' `area_correction_factor` values prior to aggregation. Non-spatial records
#' are assumed to require no adjustment and are assigned an
#' `area_correction_factor` of 1.
#'
#' The output is structured to be consistent with boundary-level rewetting
#' summaries, using a synthetic boundary representing the Scotland land area
#' (`land_area:::mhw`).
#'
#' @param rewetting_sf An `sf` object containing rewetting records, including
#'   the variables `source`, `year`, `area_ha`, and
#'   `area_correction_factor`.
#' @param non_spatial_path Character scalar. Path to an Excel workbook
#'   containing additional non-spatial rewetting records.
#'
#' @return A tibble containing:
#'
#' * `boundary_key` - Boundary identifier (`"land_area:::mhw"`).
#' * `boundary_class` - Boundary class (`"land_area"`).
#' * `boundary_name` - Boundary name (`"mhw"`).
#' * `source` - Rewetting data source.
#' * `year` - Year associated with the rewetting activity.
#' * `area_ha` - Total rewetting area (hectares).
#'
#' @details
#' This function provides a Scotland-wide summary equivalent to the outputs
#' generated by [summarise_rewetting_by_boundary()], allowing national totals
#' to be analysed alongside summaries for other boundary geographies.
summarise_rewetting_land_area <- function(rewetting_sf, non_spatial_path) {
  
  rewetting_sf |>
    sf::st_drop_geometry() |>
    bind_rows(
      openxlsx::read.xlsx(non_spatial_path) |>
        mutate(area_correction_factor = 1)
    ) |> 
    mutate(area_ha = area_ha * area_correction_factor) |> 
    dplyr::group_by(
      source,
      year
    ) |>
    dplyr::summarise(
      area_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      boundary_class = "land_area",
      boundary_name = "mhw",
      boundary_key = "land_area:::mhw",
      .before = source
    )
}

#' Summarise Peatland ACTION restoration by boundary geography
#'
#' Intersects Peatland ACTION restoration footprints with a boundary dataset
#' and calculates restored area within each boundary unit. Restoration areas
#' are adjusted using both spatial area correction factors and the proportion
#' of each grant allocated to a given financial year before aggregation.
#'
#' The output contains one row per combination of boundary, financial year,
#' delivery partner, spatial data availability class, and quality assurance
#' status, together with the total restored area assigned to that group.
#'
#' To improve performance, restoration geometries are first cropped to the
#' bounding box of the boundary dataset before spatial intersection.
#'
#' @param pa An `sf` object containing Peatland ACTION restoration geometries
#'   and associated attributes, including `financial_year_end`,
#'   `delivery_partner`, `spat_data_class`, `qa_area_match`,
#'   `overlap_5_perc`, `frac_restored_in_year`, and
#'   `area_correction_factor`.
#' @param boundary_path Character scalar. Path to a boundary dataset readable
#'   by [sf::st_read()]. The dataset must contain a `boundary_key` field in
#'   the format `"boundary_class:::boundary_name"`.
#'
#' @return A tibble containing:
#'
#' * `boundary_class` - Boundary geography type.
#' * `boundary_name` - Boundary name.
#' * `boundary_key` - Unique boundary identifier.
#' * `financial_year_end` - Financial year of restoration activity.
#' * `delivery_partner` - Delivery partner associated with the restoration.
#' * `spat_data_class` - Spatial data availability classification.
#' * `qa_area_match` - Indicator of agreement between mapped and reported
#'   restoration area.
#' * `overlap_5_perc` - Indicator of substantial overlap with another
#'   restoration footprint.
#' * `area_ha` - Total restored area (hectares) within the boundary.
#'
#' @details
#' Areas are calculated from the intersected geometries and multiplied by both
#' `area_correction_factor` and `frac_restored_in_year`. This allows restored
#' area to be correctly allocated where footprint geometries have been cropped
#' and where restoration activity associated with a single grant spans
#' multiple financial years.
summarise_pa_by_boundary <- function(
    pa,
    boundary_path
){
  
  boundary <- sf::st_read(boundary_path, quiet = TRUE) |>
    dplyr::select(boundary_key)
  
  pa <- pa |>
    dplyr::select(
      financial_year_end,
      delivery_partner,
      spat_data_class,
      qa_area_match,
      overlap_5_perc,
      frac_restored_in_year,
      area_correction_factor
    ) |>
    sf::st_crop(sf::st_bbox(boundary))
  
  sf::st_intersection(
    pa,
    boundary
  ) |>
    dplyr::mutate(
      area_ha =
        (as.numeric(sf::st_area(geom)) / 10000) *
        area_correction_factor *
        frac_restored_in_year
    ) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(
      boundary_key,
      financial_year_end,
      delivery_partner,
      spat_data_class,
      qa_area_match,
      overlap_5_perc
    ) |>
    dplyr::summarise(
      area_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    tidyr::separate_wider_delim(
      boundary_key,
      delim = ":::",
      names = c("boundary_class", "boundary_name")
    )
  
}

#' Summarise Peatland ACTION restoration for the Scotland land area
#'
#' Produces Scotland-wide summaries of Peatland ACTION restoration activity by
#' aggregating restoration areas across all records, irrespective of spatial
#' location. Restoration areas are allocated to financial years using the
#' proportion of each grant delivered in each year.
#'
#' The output is structured to be consistent with the boundary-level summaries
#' produced by [summarise_pa_by_boundary()], using a synthetic boundary
#' representing the Scotland land area (`land_area:::mhw`).
#'
#' The output contains one row per combination of financial year, delivery
#' partner, spatial data availability class, and quality assurance status,
#' together with the corresponding restored area.
#'
#' @param pa An `sf` object containing Peatland ACTION restoration records and
#'   associated attributes, including `financial_year_end`,
#'   `delivery_partner`, `spat_data_class`, `qa_area_match`,
#'   `overlap_5_perc`, `frac_restored_in_year`, and
#'   `total_ha_restored`.
#'
#' @return A tibble containing:
#'
#' * `boundary_class` - Boundary class (`"land_area"`).
#' * `boundary_name` - Boundary name (`"mhw"`).
#' * `boundary_key` - Boundary identifier (`"land_area:::mhw"`).
#' * `financial_year_end` - Financial year of restoration activity.
#' * `delivery_partner` - Delivery partner associated with the restoration.
#' * `spat_data_class` - Spatial data availability classification.
#' * `qa_area_match` - Indicator of agreement between mapped and reported
#'   restoration area.
#' * `overlap_5_perc` - Indicator of substantial overlap with another
#'   restoration footprint.
#' * `area_ha` - Total restored area (hectares).
#'
#' @details
#' Restored area is calculated as `total_ha_restored *
#' frac_restored_in_year`, allowing grants that span multiple financial years
#' to be allocated proportionately through time. The resulting output provides
#' national totals that can be analysed alongside summaries for other boundary
#' geographies.
summarise_pa_land_area <- function(
    pa
){
  
  pa |>
    st_drop_geometry() |> 
    dplyr::select(
      financial_year_end,
      delivery_partner,
      spat_data_class,
      qa_area_match,
      overlap_5_perc,
      frac_restored_in_year,
      total_ha_restored,
    ) |>
    dplyr::mutate(
      area_ha = total_ha_restored *
        frac_restored_in_year
    ) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(
      financial_year_end,
      delivery_partner,
      spat_data_class,
      qa_area_match,
      overlap_5_perc
    ) |>
    dplyr::summarise(
      area_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      boundary_class = "land_area",
      boundary_name = "mhw",
      boundary_key = "land_area:::mhw",
      .before = financial_year_end
    )
    
  
}