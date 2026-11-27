# Check the pipeline is valid, without running it
targets::tar_validate()

# Run the unit tests
library(tidyverse)
targets::tar_source()
testthat::test_dir("tests/testthat")
