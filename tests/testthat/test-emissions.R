test_that("baseline emissions are calculated correctly", {
  
  condition_df <- tibble::tibble(
    boundary_key = "land_area:::mhw",
    boundary_class = "land_area",
    boundary_name = "mhw",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    condition = "near natural bog",
    area_ha = 100,
    peat_extent_ha = 100,
    land_area_ha = 1000
  )
  
  EF_df <- tibble::tibble(
    condition = "near natural bog",
    EF = 2,
    SE = 0.5
  )
  
  out <- create_baseline_emissions_dataset(
    condition_df,
    EF_df
  )
  
  row <- dplyr::filter(
    out,
    condition == "near natural bog"
  )
  
  expect_equal(nrow(row), 1)
  
  expect_equal(row$emissions_central, 200)
  expect_equal(row$approx_lower_bound, 100)
  expect_equal(row$approx_upper_bound, 300)
  
})

test_that("total emissions row is created", {
  
  condition_df <- tibble::tibble(
    boundary_key = "land_area:::mhw",
    boundary_class = "land_area",
    boundary_name = "mhw",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    condition = "near natural bog",
    area_ha = 100,
    peat_extent_ha = 100,
    land_area_ha = 1000
  )
  
  EF_df <- tibble::tibble(
    condition = "near natural bog",
    EF = 2,
    SE = 0.5
  )
  
  out <- create_baseline_emissions_dataset(
    condition_df,
    EF_df
  )
  
  expect_true(
    "total" %in% out$condition
  )
  
})

test_that("only land area summaries are retained", {
  
  condition_df <- tibble::tibble(
    boundary_key = "la:::test",
    boundary_class = "local authority",
    boundary_name = "test",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    condition = "near natural bog",
    area_ha = 100,
    peat_extent_ha = 100,
    land_area_ha = 1000
  )
  
  EF_df <- tibble::tibble(
    condition = "near natural bog",
    EF = 2,
    SE = 0.5
  )
  
  out <- create_baseline_emissions_dataset(
    condition_df,
    EF_df
  )
  
  expect_equal(nrow(out), 0)
  
})

test_that("forest to bog restoration is excluded", {
  
  areas <- tibble::tibble(
    source = "test",
    area = 100,
    baseline_condition = "forest",
    rewetted_condition = "near natural bog"
  )
  
  EF_df <- tibble::tibble(
    condition = "near natural bog",
    EF = 1,
    SE = 0.1
  )
  
  out <- create_rewetting_emissions_ex_forestry_dataset(
    areas,
    EF_df
  )
  
  expect_false(
    "forest" %in% out$baseline_condition
  )
  
})

test_that("total emissions uncertainty is propagated correctly", {
  
  condition_df <- tibble::tibble(
    boundary_key = "land_area:::mhw",
    boundary_class = "land_area",
    boundary_name = "mhw",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    condition = c(
      "near natural bog",
      "modified bog"
    ),
    area_ha = c(100, 200),
    peat_extent_ha = 300,
    land_area_ha = 1000
  )
  
  EF_df <- tibble::tibble(
    condition = c(
      "near natural bog",
      "modified bog"
    ),
    EF = c(2, 3),
    SE = c(1, 2)
  )
  
  out <- create_baseline_emissions_dataset(
    condition_df,
    EF_df
  )
  
  total <- dplyr::filter(
    out,
    condition == "total"
  )
  
  expected_se <- sqrt(
    (100 * 1)^2 +
      (200 * 2)^2
  )
  
  expect_equal(
    total$emissions_central,
    800
  )
  
  expect_equal(
    total$approx_lower_bound,
    800 - 2 * expected_se
  )
  
  expect_equal(
    total$approx_upper_bound,
    800 + 2 * expected_se
  )
  
})

test_that("rewetting uncertainty is propagated correctly", {
  
  areas <- tibble::tibble(
    source = "test",
    area = 100,
    baseline_condition = "modified bog",
    rewetted_condition = "near natural bog"
  )
  
  EF_df <- tibble::tibble(
    condition = c(
      "modified bog",
      "near natural bog"
    ),
    EF = c(10, 2),
    SE = c(3, 4)
  )
  
  out <- create_rewetting_emissions_ex_forestry_dataset(
    areas,
    EF_df
  )
  
  row <- dplyr::filter(
    out,
    source == "test"
  )
  
  expect_equal(
    row$delta_SE,
    sqrt(3^2 + 4^2)
  )
  
})

test_that("total rewetting emissions are calculated correctly", {
  
  areas <- tibble::tibble(
    source = c("A", "B"),
    area = c(100, 100),
    baseline_condition = c(
      "modified bog",
      "modified bog"
    ),
    rewetted_condition = c(
      "near natural bog",
      "near natural bog"
    )
  )
  
  EF_df <- tibble::tibble(
    condition = c(
      "modified bog",
      "near natural bog"
    ),
    EF = c(10, 2),
    SE = c(1, 1)
  )
  
  out <- create_rewetting_emissions_ex_forestry_dataset(
    areas,
    EF_df
  )
  
  total <- dplyr::filter(
    out,
    source == "total"
  )
  
  expect_equal(
    total$area,
    200
  )
  
  expect_equal(
    total$delta_emissions_central,
    -1600
  )
  
})