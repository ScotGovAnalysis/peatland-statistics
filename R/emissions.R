#' Calculate baseline peatland emissions and uncertainty estimates
#'
#' Combines baseline peat condition area estimates with greenhouse gas
#' emission factors and calculates baseline emissions for Scotland under
#' alternative peat depth definitions.
#'
#' The function restricts the input dataset to national-level summaries
#' (`boundary_class == "land_area"`) and to peat soils defined using
#' 30 cm, 40 cm and 50 cm thresholds:
#'
#' * `peat_soil_30`
#' * `peat_soil_40`
#' * `peat_soil_50`
#'
#' Emission factors are joined to the condition dataset and multiplied by
#' condition areas to estimate emissions. Approximate lower and upper
#' uncertainty bounds are calculated using the reported emission factor
#' standard errors. In addition to condition-level estimates, the function
#' calculates total emissions for each extent source and peat depth class,
#' propagating emission factor uncertainty assuming independence among
#' condition classes.
#'
#' @param baseline_condition_df Tibble containing peat condition area
#'   summaries, typically produced by
#'   [apply_condition_assumptions()].
#' @param EF_df Tibble containing emission factors and associated standard
#'   errors for each peat condition class.
#'
#' @return A tibble containing condition-level and total emissions
#'   estimates. Additional variables include:
#'
#' * `emissions_central` - Central emissions estimate (`area_ha × EF`).
#' * `approx_lower_bound` - Approximate lower uncertainty bound.
#' * `approx_upper_bound` - Approximate upper uncertainty bound.
#'
#' Rows where `condition == "total"` contain aggregated emissions estimates
#' for each extent source and peat depth class.
#'
#' @details
#' Condition-level emissions are calculated as:
#'
#' `area_ha × EF`
#'
#' Approximate lower and upper bounds are calculated as:
#'
#' `area_ha × (EF ± 2 × SE)`
#'
#' For total emissions, standard errors are propagated assuming
#' independence among peat condition classes:
#'
#' `SE_total = sqrt(sum((area_ha × SE)^2))`
#'
#' Approximate uncertainty bounds for total emissions are then calculated
#' as:
#'
#' `total emissions ± 2 × SE_total`
#'
#' These bounds should be interpreted as approximate uncertainty ranges
#' derived from the reported emission factor standard errors rather than
#' formal confidence intervals.
create_baseline_emissions_dataset <- function(baseline_condition_df, EF_df) {
  
  processed <- baseline_condition_df |>
    filter(
      boundary_class == "land_area",
      peat_depth_class %in% c(
        "peat_soil_50",
        "peat_soil_40",
        "peat_soil_30"
      )
    ) |>
    select(
      -land_area_ha,
      -peat_extent_ha
    ) |>
    left_join(EF_df, by = "condition") |>
    mutate(
      emissions_central = area_ha * EF,
      approx_lower_bound = area_ha * (EF - 2 * SE),
      approx_upper_bound = area_ha * (EF + 2 * SE)
    )
  
  total <- processed |>
    group_by(
      boundary_key,
      boundary_class,
      boundary_name,
      extent_source,
      peat_depth_class
    ) |>
    summarise(
      condition = "total",
      se_total = sqrt(sum((area_ha * SE)^2)),
      area_ha = sum(area_ha),
      emissions_central = sum(emissions_central),
      approx_lower_bound = emissions_central - 2 * se_total,
      approx_upper_bound = emissions_central + 2 * se_total,
      .groups = "drop"
    )
  
  bind_rows(processed, total)
  
}
#' Create extraction emissions dataset
#'
#' Calculates off-site industrial peat extraction emissions from peat
#' extraction volumes using the UK National Inventory methodology.
#' Industrial peat volumes are converted to carbon emissions using an
#' emission factor of 0.0641 t C m^-3 and then converted to CO2 using
#' the molecular weight ratio of CO2 to C (44/12).
#'
#' @param extraction_volumes_df A data frame containing peat extraction
#'   volumes. Must include a column named `industrial_volume`
#'   representing industrial peat extraction volume (m^3).
#'
#' @return A data frame with all input columns and an additional column:
#'   \describe{
#'     \item{off-site_industrial_emissions}{Estimated off-site industrial
#'     peat extraction emissions (t CO2).}
#'   }
#'
#' @references Thomson et al. (2011). UK peat extraction emissions
#'   methodology using an emission factor of 0.0641 t C m^-3.
#'
create_extraction_emissions_dataset <- function(extraction_volumes_df){
  
  off_site_industrial_EF <- 0.0641
  
  c_to_co2_conversion <- 44/12
  
  extraction_volumes_df |> 
    mutate(off_site_industrial_emissions = industrial_volume * 
             off_site_industrial_EF *
               c_to_co2_conversion
             )
}

#' Calculate emissions reductions from peat soil rewetting
#'
#' Estimates changes in greenhouse gas emissions resulting from peat soil
#' rewetting by comparing baseline and rewetted peat condition emission
#' factors. Emissions reductions are calculated as the difference between
#' rewetted and baseline emission factors multiplied by the rewetted area.
#'
#' The function excludes forest-to-bog restoration, as this uses a Tier 3
#' calculation in the GHG inventory.
#' 
#' Approximate uncertainty bounds are calculated by propagating the reported
#' standard errors of the baseline and rewetted emission factors, assuming
#' independence.
#'
#' Summary emissions reductions are calculated for:
#'
#' * All rewetting activity (`total`)
#' * Peatland ACTION-funded rewetting (`pa_total`)
#' * Other rewetting activity (`other_rewetting_total`)
#'
#' @param rewetting_areas_df Tibble containing rewetting area estimates.
#'   Must include the following columns:
#'
#'   * `source` - Source of rewetting activity.
#'   * `area` - Rewetted area (ha).
#'   * `baseline_condition` - Peat condition prior to rewetting.
#'   * `rewetted_condition` - Assumed peat condition after rewetting.
#'   
#' @param EF_df Tibble containing emission factors (`EF`) and associated
#'   standard errors (`SE`) for peat condition classes.
#'
#' @return A tibble containing rewetting emissions estimates and summary
#'   totals. Additional variables include:
#'
#' * `delta_EF` - Change in emission factor (`EF2 - EF1`).
#' * `delta_SE` - Propagated standard error of the emission factor change.
#' * `delta_emissions_central` - Central estimate of emissions change.
#' * `delta_emissions_lower` - Approximate lower uncertainty bound.
#' * `delta_emissions_upper` - Approximate upper uncertainty bound.
#'
#' Rows where `source` equals `"total"`, `"pa_total"` or
#' `"other_rewetting_total"` contain aggregated estimates.
#'
#' @details
#' Emission factor changes are calculated as:
#'
#' `delta_EF = EF2 - EF1`
#'
#' where:
#'
#' * `EF1` is the baseline emission factor.
#' * `EF2` is the rewetted emission factor.
#'
#' Negative values indicate an emissions reduction following rewetting,
#' while positive values indicate an increase in emissions.
#'
#' Emission factor uncertainty is propagated assuming independence:
#'
#' `delta_SE = sqrt(SE1^2 + SE2^2)`
#'
#' Emissions changes are then calculated as:
#'
#' `area × delta_EF`
#'
#' Approximate uncertainty bounds are calculated as:
#'
#' `area × (delta_EF ± 2 × delta_SE)`
#'
#' For aggregated emissions reductions, uncertainty is propagated across
#' rewetting activities assuming independence:
#'
#' `SE_total = sqrt(sum((area × delta_SE)^2))`
#'
#' Aggregate uncertainty bounds are then calculated as:
#'
#' `total emissions change ± 2 × SE_total`
#'
#' These bounds should be interpreted as approximate uncertainty ranges
#' derived from reported emission factor standard errors rather than formal
#' confidence intervals.
#'
create_rewetting_emissions_ex_forestry_dataset <- function(rewetting_areas_df, EF_df){
  
  processed <- rewetting_areas_df |> 
    filter(baseline_condition != "forest") |> 
    select(source, area, baseline_condition, rewetted_condition) |> 
    left_join(EF_df,
              by = join_by(baseline_condition == condition)) |> 
    rename("EF1" = "EF",
           "SE1" = "SE") |> 
    left_join(EF_df,
              by = join_by(rewetted_condition == condition)) |> 
    rename("EF2" = "EF",
           "SE2" = "SE") |> 
    mutate(delta_EF = EF2 - EF1,
           delta_SE = sqrt(SE1^2 + SE2^2),
           delta_emissions_central = area * delta_EF,
           delta_emissions_upper = area * (delta_EF + 2*delta_SE),
           delta_emissions_lower = area * (delta_EF - 2*delta_SE))
  
  calculate_totals <- function(x, total_name){
    x |> 
      summarise(
        source = total_name,
        se_total = sqrt(sum((area * delta_SE)^2)),
        area = sum(area),
        delta_emissions_central = sum(delta_emissions_central),
        delta_emissions_lower = delta_emissions_central - 2 * se_total,
        delta_emissions_upper = delta_emissions_central + 2 * se_total,
        .groups = "drop"
      ) |> 
      select(-se_total)
  }
  
  total <- processed |>
    calculate_totals("total")
    
  
  pa_total <- processed |> 
    filter(source == "Peatland ACTION submission - July 2025") |> 
    calculate_totals("pa_total")
  
  other_rewetting_total <- processed |> 
    filter(source != "Peatland ACTION submission - July 2025") |> 
    calculate_totals("other_rewetting_total")
  
  bind_rows(processed,
            other_rewetting_total,
            pa_total,
            total)
  
}

