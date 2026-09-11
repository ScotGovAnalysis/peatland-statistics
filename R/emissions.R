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
create_baseline_emissions_dataset <- function(baseline_condition_df, EF_df){
    
    a <- baseline_condition_df |>
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
      left_join(EF_df) |>
      mutate(
        emissions_central = area_ha * EF,
        approx_lower_bound = area_ha * (EF - 2 * SE),
        approx_upper_bound = area_ha * (EF + 2 * SE)
      )
    
    total <- a |> 
      group_by(boundary_key, boundary_class, boundary_name,
               extent_source, peat_depth_class) |>
      summarise(
        condition = "total",
        area_ha = sum(area_ha),
        emissions_central = sum(area_ha * EF),
        se_total = sqrt(
          sum((area_ha * SE)^2)
        ),
        approx_lower_bound = emissions_central - 2 * se_total,
        approx_upper_bound = emissions_central + 2 * se_total
      ) |> 
      select(-se_total)
    
    bind_rows(a, total)
    
    
}


