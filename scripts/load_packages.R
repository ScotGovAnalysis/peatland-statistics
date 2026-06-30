# load packages ----------------------------------------------------------------
#
# This script loads packages from the project library managed by {renv}.
#
# If you are using this project for the first time, or if you have pulled
# the latest changes from the remote repository, run:
#
# renv::restore()
#
# This will install the required packages as recorded in renv.lock.

# core packages
library(tidyverse)
library(readxl)
library(fs) # file system operations
library(httr2) # working with web APIs
library(digest) # hashing

# spatial data analysis
library(sf) # primarily vector workflows
library(terra) # primarily raster workflows

# accessible outputs
library(sgplot)
library(aftables)
