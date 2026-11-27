
test_that("extent_from_list creates expected extent", {
  ext <- extent_from_list(list(
    xmin = 0,
    xmax = 100,
    ymin = 10,
    ymax = 20
  ))
  
  expect_equal(terra::xmin(ext), 0)
  expect_equal(terra::xmax(ext), 100)
  expect_equal(terra::ymin(ext), 10)
  expect_equal(terra::ymax(ext), 20)
})


test_that("peat depth is classified correctly", {
  
  r <- terra::rast(
    nrows = 1,
    ncols = 8,
    vals = c(0, 5, 10, 20, 30, 40, 50, NA)
  )
  
  result <- discretise_peat_depth(r)
  
  expect_equal(
    terra::values(result)[,1],
    c(0, 1, 2, 3, 4, 5, 6, 0)
  )
})

test_that("classification intervals are left closed", {
  
  r <- terra::rast(
    nrows = 1,
    ncols = 3,
    vals = c(9.999, 10, 19.999)
  )
  
  result <- discretise_peat_depth(r)
  
  expect_equal(
    terra::values(result)[,1],
    c(1, 2, 2)
  )
})

test_that("standardise_res aggregates raster", {
  
  r <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    vals = 1:16
  )
  
  out <- standardise_res(r, 2)
  
  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 2)
})

test_that("standardise_res disaggregates raster", {
  
  r <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    vals = 1:4
  )
  
  out <- standardise_res(r, 1)
  
  expect_equal(nrow(out), 4)
  expect_equal(ncol(out), 4)
})

test_that("standardise_res returns raster unchanged when resolution already matches", {
  
  r <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    vals = 1:4
  )
  
  out <- standardise_res(r, 2)
  
  expect_equal(terra::res(out), terra::res(r))
  expect_equal(terra::values(out), terra::values(r))
})

test_that("standardise_res uses modal aggregation for categorical rasters", {
  
  r <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    vals = c(1, 1,
             1, 2)
  )
  
  out <- standardise_res(
    r,
    resolution = 2,
    categorical = TRUE
  )
  
  expect_equal(
    terra::values(out)[1],
    1
  )
})

test_that("unzip_to_temp extracts files", {
  
  zipfile <- tempfile(fileext = ".zip")
  
  txt <- tempfile(fileext = ".txt")
  writeLines("hello", txt)
  
  utils::zip(zipfile, txt)
  
  out_dir <- unzip_to_temp(zipfile)
  
  expect_true(fs::dir_exists(out_dir))
  expect_equal(length(list.files(out_dir)), 1)
})

test_that("standardise_ext applies requested extent", {
  
  r <- terra::rast(
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10,
    nrows = 10,
    ncols = 10
  )
  
  out <- standardise_ext(
    r,
    list(
      xmin = 2,
      xmax = 8,
      ymin = 2,
      ymax = 8
    )
  )
  
  expect_equal(terra::ext(out), terra::ext(2, 8, 2, 8))
})
