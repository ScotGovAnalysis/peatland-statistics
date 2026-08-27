
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
    c(0, 1, 2, 3, 4, 5, 6, NA)
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

test_that("unzip_to_temp extracts files", {
  
  zipfile <- tempfile(fileext = ".zip")
  
  txt <- tempfile(fileext = ".txt")
  writeLines("hello", txt)
  
  utils::zip(zipfile, txt)
  
  out_dir <- unzip_to_temp(zipfile)
  
  expect_true(fs::dir_exists(out_dir))
  expect_equal(length(list.files(out_dir)), 1)
})

test_that("apply_land_area_mask handles internal and external NAs correctly", {
  
  r <- terra::rast(
    nrows = 4,
    ncols = 4,
    vals = c(
      1,  2, NA,  4,
      5, NA,  7,  8,
      9, 10, 11, 12,
      13, 14, 15, 16
    )
  )
  
  mask <- terra::rast(r)
  
  terra::values(mask) <- c(
    1, 1, 1, 1,
    1, 1, 1, 1,
    1, 1, NA, NA,
    1, 1, NA, NA
  )
  
  mask_path <- tempfile(fileext = ".tif")
  terra::writeRaster(mask, mask_path, overwrite = TRUE)
  
  result <- apply_land_area_mask(r, mask_path)
  
  expect_equal(
    terra::values(result)[, 1],
    c(
      1,  2, 0,  4,
      5,  0, 7,  8,
      9, 10, NA, NA,
      13, 14, NA, NA
    )
  )
  
})
