
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openqalyregimen

<!-- badges: start -->

[![CircleCI build
status](https://circleci.com/gh/PolicyAnalysisInc/openqalyregimen.svg?style=svg)](https://app.circleci.com/pipelines/github/PolicyAnalysisInc/openqalyregimen)
[![Codecov test
coverage](https://codecov.io/gh/PolicyAnalysisInc/openqalyregimen/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PolicyAnalysisInc/openqalyregimen?branch=master)
<!-- badges: end -->

## Overview

The openqalyregimen package provides a declarative interface for
defining treatment regimens and calculating medication costs. It handles
a wide range of cases including: - Dosing per patient, per m2 of BSA, or
per kg of weight - Oral medications with dispensing costs and oral
wastage - IV and injection medications with administration costs, vials
wastage, dose rounding - Handling of loading doses, dose escalation, and
sequencing - Correctly attributes all costs to the model cycle schedule,
even when medication cycles do not align with model cycles - Extensive
debugging output to ensure transparency - Out of the box compatability
with openqaly models, but can also be used independently

## Installation

``` r
# Install the development version from GitHub
remotes::install_github("PolicyAnalysisInc/openqalyregimen")
```

## Basic Example

``` r
library(openqalyregimen)

reg <- define_regimen(
  name = "Capecitabine",
  route = "oral",
  dose_per_admin = 2000,
  dose_basis = "bsa",
  patient_bsa = 1.8,
  patient_bsa_sd = 0.2,
  med_cycle_length = 21,
  admin_days = 1:14,
  max_med_cycles = 4,
  available_strengths = c(150, 500),
  unit_cost = 2.21,
  admin_cost = 45
)

calculate_med_costs(
  reg,
  model_cycle_length = 21,
  n_cycles = 4
)
#>   cycle cycle_start_day cycle_end_day n_dispensing_events n_tablets_dispensed
#> 1     1               0            21                   1                 112
#> 2     2              21            42                   1                 112
#> 3     3              42            63                   1                 112
#> 4     4              63            84                   1                 112
#>   drug_cost admin_cost total_cost
#> 1  275.3655         45   320.3655
#> 2  275.3655         45   320.3655
#> 3  275.3655         45   320.3655
#> 4  275.3655         45   320.3655
```

## Learn more

The package vignettes cover:

-   Oral regimens
-   IV and injection regimens
-   Combinations, loading doses, and phase changes
-   Use inside an `openqaly` model
