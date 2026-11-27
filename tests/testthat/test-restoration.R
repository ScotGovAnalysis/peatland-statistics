test_that("spatial data classes are assigned correctly", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = c("A", "B", "C"),
    financial_year_end = 2024,
    hectares = 10,
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 27700)
  
  footprints <- sf::st_sf(
    grant_id = "A",
    geom = geom
  )
  
  centroids <- sf::st_sf(
    grant_id = c("A", "B"),
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 27700
    )
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(footprints, footprints_path, delete_dsn = TRUE)
  sf::write_sf(centroids, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  )
  
  expect_equal(
    out$spat_data_class[match("A", out$grant_id)],
    "footprint"
  )
  
  expect_equal(
    out$spat_data_class[match("B", out$grant_id)],
    "centroids only"
  )
  
  expect_equal(
    out$spat_data_class[match("C", out$grant_id)],
    "none"
  )
  
})

test_that("restoration fractions sum to one within grant", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = c("A", "A"),
    financial_year_end = c(2023, 2024),
    hectares = c(10, 30),
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  empty_sf <- sf::st_sf(
    grant_id = character(),
    geom = sf::st_sfc(crs = 27700)
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(empty_sf, footprints_path, delete_dsn = TRUE)
  sf::write_sf(empty_sf, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  )
  
  expect_equal(
    sum(out$frac_restored_in_year),
    1
  )
  
})

test_that("grant IDs with suffixes are harmonised", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = "500963a",
    financial_year_end = 2024,
    hectares = 10,
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  empty_sf <- sf::st_sf(
    grant_id = character(),
    geom = sf::st_sfc(crs = 27700)
  )
  
  centroids <- sf::st_sf(
    grant_id = "500963",
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 27700
    )
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(empty_sf, footprints_path, delete_dsn = TRUE)
  sf::write_sf(centroids, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  )
  
  expect_equal(
    out$grant_id,
    "500963"
  )
  
})

test_that("footprint area approximately matches target area", {
  
  pt <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 27700
  )
  
  land <- sf::st_as_sf(
    sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = -10000,
          ymin = -10000,
          xmax = 10000,
          ymax = 10000
        ),
        crs = 27700
      )
    )
  )
  
  fp <- create_footprint(
    geom = pt,
    target_ha = 100,
    clip_geom = land
  )
  
  expect_equal(
    as.numeric(sf::st_area(fp)) / 10000,
    100,
    tolerance = 0.1
  )
  
})

test_that("restoration area is allocated across years", {
  
  pa <- sf::st_sf(
    financial_year_end = 2024,
    delivery_partner = "partner",
    spat_data_class = "footprint",
    qa_area_match = TRUE,
    overlap_10_perc = FALSE,
    frac_restored_in_year = 0.4,
    total_ha_restored = 100,
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 27700
    )
  )
  
  out <- summarise_pa_land_area(pa)
  
  expect_equal(
    out$area_ha,
    40
  )
  
  expect_equal(
    out$boundary_key,
    "land_area:::mhw"
  )
  
})

test_that("restoration area is aggregated within grant-year", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = c("A", "A"),
    financial_year_end = c(2024, 2024),
    hectares = c(10, 5),
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  empty_sf <- sf::st_sf(
    grant_id = character(),
    geom = sf::st_sfc(crs = 27700)
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(empty_sf, footprints_path, delete_dsn = TRUE)
  sf::write_sf(empty_sf, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  )
  
  expect_equal(nrow(out), 1)
  
  expect_equal(
    out$hectares,
    15
  )
  
})

test_that("grant IDs are unchanged if no matching base ID exists", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = "500963a",
    financial_year_end = 2024,
    hectares = 10,
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  empty_sf <- sf::st_sf(
    grant_id = character(),
    geom = sf::st_sfc(crs = 27700)
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(empty_sf, footprints_path, delete_dsn = TRUE)
  sf::write_sf(empty_sf, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  )
  
  expect_equal(
    out$grant_id,
    "500963a"
  )
  
})

test_that("multipoint footprints match target area", {
  
  geom <- sf::st_sfc(
    sf::st_multipoint(
      matrix(
        c(
          0, 0,
          1000, 1000
        ),
        byrow = TRUE,
        ncol = 2
      )
    ),
    crs = 27700
  )
  
  land <- sf::st_as_sf(
    sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = -10000,
          ymin = -10000,
          xmax = 10000,
          ymax = 10000
        ),
        crs = 27700
      )
    )
  )
  
  fp <- create_footprint(
    geom = geom,
    target_ha = 100,
    clip_geom = land
  )
  
  expect_equal(
    as.numeric(sf::st_area(fp)) / 10000,
    100,
    tolerance = 0.1
  )
  
})

test_that("fraction restored in year is calculated correctly", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = c("A", "A"),
    financial_year_end = c(2023, 2024),
    hectares = c(10, 30),
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  empty_sf <- sf::st_sf(
    grant_id = character(),
    geom = sf::st_sfc(crs = 27700)
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(empty_sf, footprints_path, delete_dsn = TRUE)
  sf::write_sf(empty_sf, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  ) |>
    dplyr::arrange(financial_year_end)
  
  expect_equal(
    out$frac_restored_in_year,
    c(0.25, 0.75)
  )
  
})

test_that("footprint takes precedence over centroid", {
  
  tmp <- tempdir()
  
  non_spatial <- tibble::tibble(
    grant_id = "A",
    financial_year_end = 2024,
    hectares = 10,
    delivery_partner = "partner"
  )
  
  non_spatial_path <- fs::path(tmp, "non_spatial.rds")
  saveRDS(non_spatial, non_spatial_path)
  
  geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 27700)
  
  footprints <- sf::st_sf(
    grant_id = "A",
    geom = geom
  )
  
  centroids <- sf::st_sf(
    grant_id = "A",
    geom = geom
  )
  
  footprints_path <- fs::path(tmp, "footprints.gpkg")
  centroids_path <- fs::path(tmp, "centroids.gpkg")
  
  sf::write_sf(footprints, footprints_path, delete_dsn = TRUE)
  sf::write_sf(centroids, centroids_path, delete_dsn = TRUE)
  
  out <- categorise_pa_by_spatial_data_availability(
    non_spatial_path,
    footprints_path,
    centroids_path
  )
  
  expect_equal(
    out$spat_data_class,
    "footprint"
  )
  
})

test_that("clipped footprints still match target area", {
  
  pt <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 27700
  )
  
  land <- sf::st_as_sf(
    sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = -600,
          ymin = -600,
          xmax = 600,
          ymax = 600
        ),
        crs = 27700
      )
    )
  )
  
  fp <- create_footprint(
    pt,
    target_ha = 100,
    clip_geom = land
  )
  
  expect_equal(
    as.numeric(sf::st_area(fp)) / 10000,
    100,
    tolerance = 0.1
  )
  
})

test_that("peatland action summaries aggregate correctly", {
  
  pa <- sf::st_sf(
    financial_year_end = c(2024, 2024),
    delivery_partner = c("A", "A"),
    spat_data_class = c("footprint", "footprint"),
    qa_area_match = c(TRUE, TRUE),
    overlap_10_perc = c(FALSE, FALSE),
    frac_restored_in_year = c(1, 1),
    total_ha_restored = c(10, 15),
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 27700
    )
  )
  
  out <- summarise_pa_land_area(pa)
  
  expect_equal(
    out$area_ha,
    25
  )
  
})