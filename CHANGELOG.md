# Changelog

All notable changes to the project will be documented in this file. 

The format is based on [Keep a Changelog](https://keepachangelog.com/en/2.0.0/).

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Currently, the project is in early development (major version zero). Anything
may change at any time, and the project is not considered stable.

Dates follow YYYY-MM-DD format.

## 0.1.2 2026-07-02

## Removed
- scripts folder and content: not required
- dependencies will be managed via the _targets script and namespaces.

## Changed
- download_data functions completed
- _targets.R pipeline configured correctly
- readme to reflect above changes

## 0.1.1 2026-07-01

### Added
- targets_R, to use [targets](https://books.ropensci.org/targets/) package:
"The targets package is a Make-like pipeline tool for statistics and data science in R.
The package skips costly runtime for tasks that are already up to date, orchestrates the necessary computation with implicit parallel computing, and abstracts files as R objects.
If all the current output matches the current upstream code and data, then the whole pipeline is up to date, and the results are more trustworthy than otherwise."
- R/download_data.R, function to download publicly available datasets
- config/public_input_data_catalogue, an input data catalogue which acts as a table of parameters for the download function

### Removed
- pipeline.R as now defunct

## 0.1.0 2026-06-30

### Added
- pipeline.R, a main project control script
- load_packages.R, a script to load required libraries
- installed packages and updated renv lockfile

