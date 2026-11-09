test_that("validation passes when all download datasets are present", {
  config <- list(
    input_datasets = list(
      a = list(fun = "download"),
      b = list(fun = "arcgol_api"),
      c = list(fun = "read_rds")
    )
  )
  
  catalogue <- tibble::tibble(dataset = c("a", "b"))
  
  expect_true(
    validate_public_data_catalogue_datasets(config, catalogue)
  )
})

test_that("validation fails when configured dataset missing", {
  config <- list(
    input_datasets = list(
      a = list(fun = "download"),
      b = list(fun = "arcgol_api")
    )
  )
  
  catalogue <- tibble::tibble(dataset = "a")
  
  expect_error(
    validate_public_data_catalogue_datasets(config, catalogue),
    "Datasets missing from catalogue: b"
  )
})

test_that("non-download datasets are ignored", {
  config <- list(
    input_datasets = list(
      a = list(fun = "read_rds")
    )
  )
  
  catalogue <- tibble::tibble(dataset = character())
  
  expect_true(
    validate_public_data_catalogue_datasets(config, catalogue)
  )
})

test_that("all missing datasets are reported", {
  
  config <- list(
    input_datasets = list(
      a = list(fun = "download"),
      b = list(fun = "download"),
      c = list(fun = "download")
    )
  )
  
  catalogue <- tibble::tibble(dataset = "a")
  
  expect_error(
    validate_public_data_catalogue_datasets(config, catalogue),
    "b, c"
  )
})

test_that("errors if dataset not found", {
  
  catalogue <- tibble::tibble(
    dataset = "other"
  )
  
  expect_error(
    download_dataset("test", catalogue),
    "Dataset 'test' not found"
  )
})

test_that("errors if dataset appears multiple times", {
  
  catalogue <- tibble::tibble(
    dataset = c("test", "test")
  )
  
  expect_error(
    download_dataset("test", catalogue),
    "appears 2 times"
  )
})


test_that("returns NULL when metadata location is missing", {
  
  expect_null(
    download_metadata(
      NA_character_,
      "test",
      tempdir(),
      ".xml"
    )
  )
})

test_that("returns NULL when metadata location is empty", {
  
  expect_null(
    download_metadata(
      "",
      "test",
      tempdir(),
      ".xml"
    )
  )
})

test_that("returns path when file exists", {
  
  dir <- fs::dir_create(
    fs::path(tempdir(), "data", "raw", "test")
  )
  
  file <- fs::file_create(
    fs::path(dir, "file.csv")
  )
  
  old <- setwd(tempdir())
  on.exit(setwd(old))
  
  expect_equal(
    verify_dataset("test", "file.csv"),
    fs::path("data", "raw", "test", "file.csv")
  )
})

test_that("errors when file does not exist", {
  
  expect_error(
    verify_dataset("test", "missing.csv"),
    "File not found"
  )
})
