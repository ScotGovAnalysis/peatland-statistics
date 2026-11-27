validate_rewetting <- function(df){
  
  # No negative rewetting areas
  
  if (any(df$area_ha < 0, na.rm = TRUE)) {
    stop("Negative rewetting areas detected.")
  }
  
  # No missing key variables
  
  required_cols <- c(
    "boundary_key",
    "boundary_class",
    "boundary_name",
    "source",
    "year",
    "area_ha"
  )
  
  if (anyNA(df[required_cols])) {
    stop("Missing values found in key rewetting fields.")
  }
  
  # boundary key consistent with components
  
  check <- df |>
    mutate(
      expected_key =
        paste0(boundary_class, ":::", boundary_name)
    )
  
  if (!all(check$boundary_key == check$expected_key)) {
    stop("boundary_key inconsistent with boundary_class/name.")
  }
  
  # no duplicate summaries
  
  dupes <- df |>
    count(
      boundary_key,
      source,
      year
    ) |>
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    stop("Duplicate rewetting summary rows detected.")
  }
  
  # land area summary exists
  
  if (!any(df$boundary_key == "land_area:::mhw")) {
    stop("Land area summary missing.")
  }
}

validate_restoration <- function(df){
  
  # No negative restoration areas
  
  if (any(df$area_ha < 0, na.rm = TRUE)) {
    stop("Negative restoration areas detected.")
  }
  
  # No missing key variables
  
  required_cols <- c(
    "boundary_key",
    "boundary_class",
    "boundary_name",
    "financial_year_end",
    "delivery_partner",
    "spat_data_class",
    "area_ha"
  )
  
  if (anyNA(df[required_cols])) {
    stop("Missing values found in key restoration fields.")
  }
  
  # boundary key consistent with components
  
  check <- df |>
    mutate(
      expected_key =
        paste0(boundary_class, ":::", boundary_name)
    )
  
  if (!all(check$boundary_key == check$expected_key)) {
    stop("boundary_key inconsistent with boundary_class/name.")
  }
  
  # no duplicate summaries
  
  dupes <- df |>
    count(
      boundary_key,
      financial_year_end,
      delivery_partner,
      spat_data_class,
      qa_area_match,
      overlap_10_perc
    ) |>
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    stop("Duplicate restoration summary rows detected.")
  }
  
  # land area summary exists
  
  if (!any(df$boundary_key == "land_area:::mhw")) {
    stop("Land area summary missing.")
  }
  
  # valid spatial data classes
  
  valid_spat_classes <- c(
    "footprint",
    "centroids only",
    "none"
  )
  
  if (!all(df$spat_data_class %in% valid_spat_classes)) {
    stop("Unexpected spatial data class detected.")
  }
  
}

validate_baseline_condition <- function(df){
  
  # No negative areas
  
  if (any(df$area_ha < 0, na.rm = TRUE)) {
    stop("Negative condition areas detected.")
  }
  
  # No missing key variables
  
  required_cols <- c(
    "boundary_key",
    "boundary_class",
    "boundary_name",
    "extent_source",
    "peat_depth_class",
    "condition",
    "land_area_ha"
  )
  
  if (anyNA(df[required_cols])) {
    stop("Missing values found in key condition fields.")
  }
  
  # boundary key consistent with components
  
  check <- df |>
    mutate(
      expected_key =
        paste0(boundary_class, ":::", boundary_name)
    )
  
  if (!all(check$boundary_key == check$expected_key)) {
    stop("boundary_key inconsistent with boundary_class/name.")
  }
  
  # no duplicate summaries
  
  dupes <- df |>
    count(
      boundary_key,
      extent_source,
      peat_depth_class,
      condition
    ) |>
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    stop("Duplicate condition summary rows detected.")
  }
  
  # condition areas sum to peat extent area
  
  totals <- df |>
    group_by(
      boundary_key,
      extent_source,
      peat_depth_class
    ) |>
    summarise(
      condition_area = sum(area_ha),
      peat_extent_ha = first(peat_extent_ha),
      .groups = "drop"
    ) |> 
    filter(!is.na(condition_area) & is.na(peat_extent_ha))
  
  if (
    any(
      abs(totals$condition_area - totals$peat_extent_ha) > 1e-6
    )
  ) {
    stop("Condition areas do not sum to peat extent area.")
  }
  
  # peat extent cannot exceed land area
  
  if (any(df$peat_extent_ha > df$land_area_ha, na.rm = TRUE)) {
    stop("Peat extent exceeds land area.")
  }
  
  # land area summary exists
  
  if (!any(df$boundary_key == "land_area:::mhw")) {
    stop("Land area summary missing.")
  }
  
}

validate_simplified_condition_time_series <- function(df){
  
  # No negative areas
  
  if (any(df$degraded < 0, na.rm = TRUE)) {
    stop("Negative degraded area detected.")
  }
  
  if (any(df$rewetted < 0, na.rm = TRUE)) {
    stop("Negative rewetted area detected.")
  }
  
  if (any(df$`near natural` < 0, na.rm = TRUE)) {
    stop("Negative near natural area detected.")
  }
  
  # No missing key variables
  
  required_cols <- c(
    "boundary_key",
    "boundary_class",
    "boundary_name",
    "extent_source",
    "peat_depth_class",
    "year"
  )
  
  if (anyNA(df[required_cols])) {
    stop("Missing values found in key condition time series fields.")
  }
  
  # boundary key consistent with components
  
  check <- df |>
    mutate(
      expected_key =
        paste0(boundary_class, ":::", boundary_name)
    )
  
  if (!all(check$boundary_key == check$expected_key)) {
    stop("boundary_key inconsistent with boundary_class/name.")
  }
  
  # no duplicate summaries
  
  dupes <- df |>
    count(
      boundary_key,
      extent_source,
      peat_depth_class,
      year
    ) |>
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    stop("Duplicate condition time series rows detected.")
  }
  
  # rewetted area cannot exceed original degraded area
  
  if (any(df$rewetted > df$peat_extent_ha, na.rm = TRUE)) {
    stop("Rewetted area exceeds peat extent.")
  }
  
  # total peat extent preserved through time
  
  totals <- df |>
    filter(!if_any(everything(), is.na)) |> 
    mutate(
      total = `near natural` + degraded + rewetted
    )
  
  if (any(abs(totals$total - totals$peat_extent_ha) > 1e-6)) {
    stop("Peat extent is not preserved through time.")
  }
  
  # baseline year present
  
  if (!any(df$year == 1990)) {
    stop("Baseline year 1990 missing.")
  }
  
}

validate_baseline_emissions <- function(df){
  
  # No missing key variables
  
  required_cols <- c(
    "boundary_key",
    "boundary_class",
    "boundary_name",
    "extent_source",
    "peat_depth_class",
    "condition"
  )
  
  if (anyNA(df[required_cols])) {
    stop("Missing values found in key emissions fields.")
  }
  
  # No duplicate summaries
  
  dupes <- df |>
    count(
      boundary_key,
      extent_source,
      peat_depth_class,
      condition
    ) |>
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    stop("Duplicate emissions summary rows detected.")
  }
  
  # boundary key consistent with components
  
  check <- df |>
    mutate(
      expected_key =
        paste0(boundary_class, ":::", boundary_name)
    )
  
  if (!all(check$boundary_key == check$expected_key)) {
    stop("boundary_key inconsistent with boundary_class/name.")
  }
  
  # uncertainty bounds must contain central estimate
  
  if (any(
    df$emissions_central < df$approx_lower_bound,
    na.rm = TRUE
  )) {
    stop("Emissions central estimate below lower bound.")
  }
  
  if (any(
    df$emissions_central > df$approx_upper_bound,
    na.rm = TRUE
  )) {
    stop("Emissions central estimate above upper bound.")
  }
  
  # exactly one total row per extent/depth combination
  
  total_rows <- df |>
    filter(condition == "total") |>
    count(
      extent_source,
      peat_depth_class
    )
  
  if (any(total_rows$n != 1)) {
    stop("Invalid number of total emissions rows detected.")
  }
  
  # total emissions reconcile
  
  detail <- df |>
    filter(condition != "total") |>
    group_by(
      extent_source,
      peat_depth_class
    ) |>
    summarise(
      area_ha = sum(area_ha),
      emissions_central = sum(emissions_central),
      .groups = "drop"
    )
  
  totals <- df |>
    filter(condition == "total") |>
    select(
      extent_source,
      peat_depth_class,
      total_area_ha = area_ha,
      total_emissions = emissions_central
    )
  
  reconciliation <- left_join(
    detail,
    totals,
    by = c(
      "extent_source",
      "peat_depth_class"
    )
  ) |> 
    filter(!if_any(everything(), is.na))
  
  if (
    any(
      abs(
        reconciliation$area_ha -
        reconciliation$total_area_ha
      ) > 1e-6
    )
  ) {
    stop("Total emissions area does not reconcile.")
  }
  
  if (
    any(
      abs(
        reconciliation$emissions_central -
        reconciliation$total_emissions
      ) > 1e-6
    )
  ) {
    stop("Total emissions do not reconcile.")
  }
  
  # national summary exists
  
  if (!any(df$boundary_key == "land_area:::mhw")) {
    stop("Land area summary missing.")
  }
  
}

validate_rewetting_emissions <- function(df){
  
  # No negative areas
  
  if (any(df$area < 0, na.rm = TRUE)) {
    stop("Negative rewetting areas detected.")
  }
  
  # No duplicate source rows
  
  dupes <- df |>
    count(source)
  
  if (any(dupes$n > 1 &
          dupes$source %in%
          c("total", "pa_total", "other_rewetting_total"))) {
    stop("Duplicate total rows detected.")
  }
  
  # Total rows present
  
  expected_totals <- c(
    "total",
    "pa_total",
    "other_rewetting_total"
  )
  
  if (!all(expected_totals %in% df$source)) {
    stop("Expected total rows missing.")
  }
  
  # Bounds contain central estimate
  
  if (
    any(
      df$delta_emissions_central <
      df$delta_emissions_lower,
      na.rm = TRUE
    )
  ) {
    stop("Central estimate below lower bound.")
  }
  
  if (
    any(
      df$delta_emissions_central >
      df$delta_emissions_upper,
      na.rm = TRUE
    )
  ) {
    stop("Central estimate above upper bound.")
  }
  
  # Grand total reconciles
  
  detail <- df |>
    filter(
      !source %in% c(
        "total",
        "pa_total",
        "other_rewetting_total"
      )
    )
  
  total_row <- df |>
    filter(source == "total")
  
  if (
    abs(
      sum(detail$area) -
      total_row$area
    ) > 1e-6
  ) {
    stop("Total rewetting area does not reconcile.")
  }
  
  if (
    abs(
      sum(detail$delta_emissions_central) -
      total_row$delta_emissions_central
    ) > 1e-6
  ) {
    stop("Total emissions reduction does not reconcile.")
  }
  
}

compare_against_original_basemap <- function(
  ghgi_condition_path,
  baseline_condition_df
){
  ghgi_orig <- terra::rast(ghgi_condition_path) 
  resolution <- terra::res(ghgi_orig) |> first()
  
  ghgi_orig_areas <- ghgi_orig |> 
    terra::freq() |> 
    mutate(area_ha = (count * resolution^2) / 10000,
           condition = value |> str_to_lower(),
           peat_extent_ha = sum(area_ha),
           .keep = "none") |> 
    mutate(boundary_key = NA,
          boundary_class = NA,
          boundary_name = NA,
          extent_source = NA,
          peat_depth_class = NA, 
          land_area_ha = NA
    ) |> 
    apply_condition_assumptions() |> 
    select(condition, area_ha, peat_extent_ha)
  
  modelled_condition_df <- baseline_condition_df |> 
    filter(extent_source == "ghgi_extent",
           peat_depth_class == "peat_soil_50",
           boundary_class == "land_area") |> 
    select(condition, area_ha, peat_extent_ha)
  
  comparison <- full_join(
    ghgi_orig_areas,
    modelled_condition_df,
    by = join_by(condition),
    suffix = c("_original", "_modelled")
  ) |> 
    mutate(area_diff = area_ha_modelled - area_ha_original,
           peat_extent_diff = peat_extent_ha_modelled - peat_extent_ha_original) |> 
    mutate(area_perc_diff = area_diff/ area_ha_original * 100,
           peat_extent_perc_diff = peat_extent_diff / peat_extent_ha_original * 100)
  
  # peat extent must agree within 0.1%
  
  if (
    any(
      abs(comparison$peat_extent_perc_diff) > 0.1,
      na.rm = TRUE
    )
  ) {
    stop(
      "Modelled peat extent differs from original GHGI basemap by >1%."
    )
  }
  
  # condition areas must agree within 1%
  
  if (
    any(
      abs(comparison$area_perc_diff) > 1,
      na.rm = TRUE
    )
  ) {
    stop(
      "Condition areas differ from original GHGI basemap by >1%."
    )
  }
  
  comparison
  
}