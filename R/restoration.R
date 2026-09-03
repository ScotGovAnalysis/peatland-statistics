
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