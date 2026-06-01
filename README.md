# peatland-statistics

Open analytical code supporting official statistics on Scotland’s peatlands.

------------------------------------------------------------------------

## Overview

The Scottish Government's Rural and Environment Science and Analytical Services division (RESAS) is [Developing Official Statistics on Scotland's Peatlands](https://www.gov.scot/publications/developing-official-statistics-on-scotlands-peatlands/).

This project contains reproducible R workflows which support the production these official statistics in development.

The project is designed following [reproducible analytical pipeline (RAP) principles](https://analysisfunction.civilservice.gov.uk/support/reproducible-analytical-pipelines/), ensuring that results are robust, traceable, and easy to maintain.

Development adheres to the [Code of Practice for Statistics](https://code.statisticsauthority.gov.uk/)

------------------------------------------------------------------------

## Project structure

-   `scripts/` – workflow scripts for data processing and analysis\
-   `R/` – reusable functions\
-   `data/` – raw, processed, and sample data\
-   `outputs/` – generated tables, charts, and spatial outputs\
-   `quality/` – unit tests and validation scripts\
-   `config/` – configuration files

------------------------------------------------------------------------

## Reproducibility

This project uses the `renv` package to manage package versions.

To recreate the R environment, run:

```         
renv::restore()
```

------------------------------------------------------------------------

## Running the analysis

Analytical workflows are organised into scripts in the `scripts/` directory. These are typically run in sequence (e.g. data acquisition → cleaning → analysis → outputs).

------------------------------------------------------------------------

## Data and outputs

Raw and processed data are not stored in this repository.

Outputs (e.g. tables, charts, spatial data) are generated locally by running the scripts provided.

Sample data may be included in `data/sample/` for demonstration and testing purposes.

------------------------------------------------------------------------

## Quality assurance

This repository includes both unit testing and high-level validation:

-   Unit tests: `quality/tests/`
-   Validation scripts: `quality/validation/`

------------------------------------------------------------------------

## Configuration

Configuration is managed through `config/config.yml`.

This file may include:

-   file paths
-   environment-specific settings
-   credentials (where appropriate)

This file is not tracked in version control. A template is provided in:

```         
config/config_template.yml
```

------------------------------------------------------------------------

## Contributing

This project is in early development.

Contributions to this project are currently managed by a small group of maintainers.

This repository follows the guidance set out in <https://github.com/ScotGovAnalysis/welcome> .

If you would like to contribute or have suggestions, please contact the project team to discuss further.

------------------------------------------------------------------------

## Coding style

This project uses the [tidyverse style](https://style.tidyverse.org/).

Functions should have multi-line [roxygen2](https://roxygen2.r-lib.org/articles/roxygen2.html)-style comments.

------------------------------------------------------------------------

## Changelog

A record of significant changes to the project is maintained in `CHANGELOG.md`.

------------------------------------------------------------------------

## Licence

This work is licensed under the Open Government Licence v3.0.

------------------------------------------------------------------------

## Contact

[environmentanalysis\@gov.scot](mailto:environmentanalysis@gov.scot)
