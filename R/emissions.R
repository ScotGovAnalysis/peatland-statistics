#' Calculate emissions from baseline peat condition data
#'
#' Combines baseline peat condition area estimates with emission factors and
#' calculates greenhouse gas emissions for Scotland under alternative peat
#' depth definitions, at the 1990 baseline.
#'
#' The function restricts the input dataset to national-level summaries
#' (`boundary_class == "land_area"`) and to peat soils, at 30, 40 and 50 cm
#' depth thresholds:
#'
#' * `peat_soil_50`
#' * `peat_soil_40`
#' * `peat_soil_30`
#'
#' Emission factors and associated confidence interval bounds are joined to
#' the condition dataset and multiplied by the corresponding area estimates to
#' calculate total emissions.
#'
#' @param baseline_condition_df Tibble containing peat condition area
#'   summaries, typically produced by
#'   [apply_condition_assumptions()].
#' @param EF_df Tibble containing emission factors and confidence interval
#'   bounds for each peat condition class.
#'
#' @return A tibble containing peat condition areas, emission factors, and
#' estimated emissions. Additional variables include:
#'
#' * `emissions_central` - Central emissions estimate.
#' * `emissions_ef_min` - Lower confidence interval estimate from EF.
#' * `emissions_ef_max` - Upper confidence interval estimate from EF.
#'
#' @details
#' Emissions are calculated as:
#'
#' `area_ha × emission factor`
#'
#' for the central estimate and each confidence interval bound. The function
#' assumes that `EF_df` contains one emission factor record for each peat
#' condition category represented in `baseline_condition_df`.
create_baseline_emissions_dataset <- function(baseline_condition_df, EF_df){
  baseline_condition_df |> 
    filter(boundary_class == "land_area",
           peat_depth_class %in% c("peat_soil_50", "peat_soil_40", "peat_soil_30")) |> 
    select(-land_area_ha, -peat_extent_ha) |> 
    left_join(EF_df) |>
    mutate(emissions_central = area_ha * EF,
           emissions_ef_min = area_ha * EF_CI_min,
           emissions_ef_max = area_ha * EF_CI_max)
}