test_that("agreement map counts peat classifications correctly", {
  
  old <- setwd(tempdir())
  on.exit(setwd(old))
  
  r1 <- terra::rast(
    nrows = 1,
    ncols = 3,
    vals = c(6, 6, 0)
  )
  
  r2 <- terra::rast(
    nrows = 1,
    ncols = 3,
    vals = c(6, 0, 6)
  )
  
  p1 <- tempfile(fileext = ".tif")
  p2 <- tempfile(fileext = ".tif")
  
  terra::writeRaster(r1, p1, overwrite = TRUE)
  terra::writeRaster(r2, p2, overwrite = TRUE)
  
  out_path <- create_agreement_map(
    c(p1, p2),
    peat_class = 6
  )
  
  out <- terra::rast(out_path)
  
  expect_equal(
    terra::values(out)[,1],
    c(2, 1, 1)
  )
})

test_that("agreement map ignores missing values", {
  
  old <- setwd(tempdir())
  on.exit(setwd(old))
  
  r1 <- terra::rast(
    nrows = 1,
    ncols = 2,
    vals = c(6, NA)
  )
  
  r2 <- terra::rast(
    nrows = 1,
    ncols = 2,
    vals = c(0, 6)
  )
  
  p1 <- tempfile(fileext = ".tif")
  p2 <- tempfile(fileext = ".tif")
  
  terra::writeRaster(r1, p1, overwrite = TRUE)
  terra::writeRaster(r2, p2, overwrite = TRUE)
  
  out <- terra::rast(
    create_agreement_map(c(p1, p2), 6)
  )
  
  expect_equal(
    terra::values(out)[,1],
    c(1, 1)
  )
})