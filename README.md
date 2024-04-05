
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortSymmetry <img src="man/figures/CSHex.png" align="right" height="139"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of CohortSymmetry is to carry out the necessary calculations
for Sequence Symmetry Analysis (SSA). It is highly recommended that this
method is tested beforehand against well-known positive and negative
controls. Such controls could be found using the paper in the Reference.
<sup>1</sup>

## Installation

You can install the development version of CohortSymmetry from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/CohortSymmetry")
```

## Example

### Create a reference to data in the OMOP CDM format

The CohortSymmetry package is designed to work with data in the OMOP CDM
(Common Data Model) format, so our first step is to create a reference
to the data using the `CDMConnector` package.

As an example, we will be using Eunomia data set.

``` r
library(CDMConnector)
library(dplyr)
library(DBI)
library(duckdb)
 
db <- DBI::dbConnect(duckdb::duckdb(), 
                     dbdir = CDMConnector::eunomia_dir())
cdm <- cdm_from_con(
  con = db,
  cdm_schema = "main",
  write_schema = "main"
)
```

### Step 0: Instantiate two cohorts in the cdm reference

This will be entirely user’s choice on how to generate such cohorts.
Minimally, this package requires two cohort tables in the cdm reference,
namely the index_cohort and the marker_cohort.

If one wants to generate two drugs cohorts in cdm, DrugUtilisation is
recommended. For merely illustration purposes, we will carry out PSSA on
aspirin (index_cohort) against amoxicillin (marker_cohort)

``` r
library(dplyr)
library(DrugUtilisation)
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "aspirin",
                                                    ingredient = "aspirin")

cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "amoxicillin",
                                                    ingredient = "amoxicillin")
```

### Step 1: generateSequenceCohortSet

In order to initiate the calculations, the two cohorts tables need to be
intersected using `CohortSymmetry::generateSequenceCohortSet()`. This
process will output all the individuals who appeared on both tables
according to a user-specified parameters. This includes `timeGap`,
`washoutWindow`, `indexMarkerGap` and `daysPriorObservation`. Details on
these parameters could be found on the vignette.

``` r
library(CohortSymmetry)
 
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  indexTable = "aspirin",
  markerTable = "amoxicillin",
  name = "aspirin_amoxicillin"
)

cdm$aspirin_amoxicillin %>% 
  dplyr::glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB 0.8.1 [xihangc@Windows 10 x64:R 4.3.1/C:\Users\xihangc\AppData\Local\Temp\RtmpUBIYbH\file4f2062731fd8.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ subject_id           <int> 65, 119, 185, 144, 235, 197, 310, 316, 331, 363, …
#> $ cohort_start_date    <date> 1968-07-29, 1967-05-28, 1947-04-07, 1978-10-30, …
#> $ cohort_end_date      <date> 1969-06-18, 1968-04-07, 1947-04-12, 1979-09-04, …
#> $ index_date           <date> 1969-06-18, 1967-05-28, 1947-04-07, 1978-10-30, …
#> $ marker_date          <date> 1968-07-29, 1968-04-07, 1947-04-12, 1979-09-04, …
```

### Step 2: summariseSequenceRatio

To get the sequence ratios, we would need the output of the
generateSequenceCohortSet() function to be fed into
`CohortSymmetry::summariseSequenceRatio()` The output of this process
contains cSR(crude sequence ratio), aSR(adjusted sequence ratio) and
confidence intervals.

``` r
res <- CohortSymmetry::summariseSequenceRatio(cdm = cdm,
                                         sequenceTable = "aspirin_amoxicillin")
#> Joining with `by = join_by(days_prior_observation, washout_window,
#> index_marker_gap, combination_window, confidence_interval,
#> moving_average_restriction, cdm_name)`
#> ! The following column type were changed: • result_id: from character to
#> integer
 
res %>% glimpse()
#> Rows: 16
#> Columns: 16
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ result_type      <chr> "sequence_ratios", "sequence_ratios", "sequence_ratio…
#> $ package_name     <chr> "CohortSymmetry", "CohortSymmetry", "CohortSymmetry",…
#> $ package_version  <chr> "0.0.0.9000", "0.0.0.9000", "0.0.0.9000", "0.0.0.9000…
#> $ group_name       <chr> "index_cohort_name and marker_cohort_name", "index_co…
#> $ group_level      <chr> "aspirin and amoxicillin", "aspirin and amoxicillin",…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "first_pharmac", "first_pharmac", "first_pharmac", "f…
#> $ variable_level   <chr> "index", "index", "marker", "marker", "crude", "adjus…
#> $ estimate_name    <chr> "count", "percentage", "count", "percentage", "point_…
#> $ estimate_type    <chr> "integer", "numeric", "integer", "numeric", "numeric"…
#> $ estimate_value   <chr> "56", "58.9", "39", "41.1", "1.43589743589744", "1.35…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Step 3: visualise the results

The user could then visualise their results using a wide array of
provided tools.

For example, the following produces a gt table.

``` r
gt_results <- CohortSymmetry::tableSequenceRatios(result = res)

gt_results
```

![](./man/figures/README-gt_table.png)

### Disconnect from the cdm database connection

``` r
CDMConnector::cdmDisconnect(cdm = cdm)
```

## References

1.  Pratt N, Chan EW, Choi NK, et al. Prescription sequence symmetry
    analysis: assessing risk, temporality, and consistency for adverse
    drug reactions across datasets in five countries. Pharmacoepidemiol
    Drug Saf. 2015;24(8):858-864.
