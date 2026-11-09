test_that("mapping offset is reclassified to near natural bog", {
  
  x <- tibble::tibble(
    boundary_key = "a",
    boundary_class = "test",
    boundary_name = "test",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    land_area_ha = 100,
    peat_extent_ha = 20,
    condition = "mapping offset",
    area_ha = 10
  )
  
  out <- apply_condition_assumptions(x)
  
  expect_equal(
    unique(out$condition),
    "near natural bog"
  )
})

test_that("modified bog is split using drainage assumptions", {
  
  x <- tibble::tibble(
    boundary_key = "a",
    boundary_class = "test",
    boundary_name = "test",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    land_area_ha = 100,
    peat_extent_ha = 20,
    condition = "modified bog",
    area_ha = 100
  )
  
  out <- apply_condition_assumptions(x)
  
  expect_equal(
    out$area_ha[out$condition == "modified bog - drained"],
    27.5
  )
  
  expect_equal(
    out$area_ha[out$condition == "modified bog - undrained"],
    72.5
  )
})

test_that("eroded bog is partitioned correctly", {
  
  x <- tibble::tibble(
    boundary_key = "a",
    boundary_class = "test",
    boundary_name = "test",
    extent_source = "test",
    peat_depth_class = "peat_soil_50",
    land_area_ha = 100,
    peat_extent_ha = 20,
    condition = "eroded",
    area_ha = 100
  )
  
  out <- apply_condition_assumptions(x)
  
  expect_equal(sum(out$area_ha), 100)
  
  expect_equal(
    out$area_ha[out$condition == "eroding bog - drained"],
    100 * 0.15 * 0.275
  )
  
  expect_equal(
    out$area_ha[out$condition == "eroding bog - undrained"],
    100 * 0.15 * 0.725
  )
})

test_that("baseline degraded area is derived correctly", {
  
  condition_df <- tibble::tibble(
    boundary_key = "a",
    peat_extent_ha = 100,
    area_ha = 20,
    condition = "near natural bog"
  )
  
  rewetting_df <- tibble::tibble(
    boundary_key = "a",
    year = 1991,
    source = "x",
    area_ha = 0
  )
  
  out <- create_simple_condition_ts(
    condition_df,
    rewetting_df
  )
  
  baseline <- dplyr::filter(out, year == 1990)
  
  expect_equal(
    baseline$degraded,
    80
  )
})

test_that("rewetting is accumulated through time", {
  
  condition_df <- tibble::tibble(
    boundary_key = "a",
    peat_extent_ha = 100,
    area_ha = 20,
    condition = "near natural bog"
  )
  
  rewetting_df <- tibble::tibble(
    boundary_key = "a",
    year = c(1991, 1992),
    source = "x",
    area_ha = c(5, 10)
  )
  
  out <- create_simple_condition_ts(
    condition_df,
    rewetting_df
  )
  
  expect_equal(
    out$rewetted[out$year == 1992],
    15
  )
})

test_that("rewetting cannot exceed degraded area", {
  
  condition_df <- tibble::tibble(
    boundary_key = "a",
    peat_extent_ha = 100,
    area_ha = 20,
    condition = "near natural bog"
  )
  
  rewetting_df <- tibble::tibble(
    boundary_key = "a",
    year = 1991,
    source = "x",
    area_ha = 200
  )
  
  out <- create_simple_condition_ts(
    condition_df,
    rewetting_df
  )
  
  expect_equal(
    out$rewetted[out$year == 1991],
    80
  )
  
  expect_equal(
    out$degraded[out$year == 1991],
    0
  )
})
