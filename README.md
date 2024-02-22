
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortSymmetry <img src="man/figures/CSHex.png" align="right" height="139"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of CohortSymmetry is to carry out the necessary calculations
for Sequence Symmetry Analysis (SSA).

## Installation

You can install the development version of CohortSymmetry from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/CohortSymmetry")
```

## Example

This is a basic example which carries out the necessary calculations for
Sequence Symmetry Analysis:

### Create a reference to data in the OMOP CDM format

The CohortSymmetry package is designed to work with data in the OMOP CDM
(Common Data Model) format, so our first step is to create a reference
to the data using the CDMConnector package.

As an example, we will be utilising the Eunomia data set.

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
Minimally, this package requires two tables in the cdm reference to be
cohort tables; in other words, the columns of these tables should
contain cohort_definition_id, subject_id, cohort_start_date and
cohort_end_date.

If one wants to generate two drugs cohorts in cdm, DrugUtilisation is
recommended. As an example, amiodarone and levothyroxine are used. This
is a known positive benchmark reported in the literature.<sup>1</sup>

``` r
library(DrugUtilisation)
library(CodelistGenerator)
library(dplyr)
 
index_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "amiodarone")
marker_drug <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "levothyroxine")
 
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort_index",
    conceptSet = index_drug
  )
 
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = "cohort_marker",
    conceptSet = marker_drug
  )
 
cdm$cohort_index %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [braventos@Windows 10 x64:R 4.1.2/C:\Users\BRAVEN~1\AppData\Local\Temp\RtmpiUNOTG\file46984e71f0e.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
#> $ subject_id           <int> 651, 712, 752, 918, 1410, 2518, 2602, 2955, 2985,~
#> $ cohort_start_date    <date> 1999-01-28, 2011-04-14, 1979-02-04, 2016-05-20, ~
#> $ cohort_end_date      <date> 1999-01-28, 2011-04-14, 1979-02-04, 2016-05-20, ~
 
cdm$cohort_marker %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [braventos@Windows 10 x64:R 4.1.2/C:\Users\BRAVEN~1\AppData\Local\Temp\RtmpiUNOTG\file46984e71f0e.duckdb]
#> $ cohort_definition_id <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
#> $ subject_id           <int> 1079, 515, 3866, 19, 2487, 2752, 4375, 4532, 3224~
#> $ cohort_start_date    <date> 2016-03-05, 2009-03-05, 2017-09-24, 2004-01-17, ~
#> $ cohort_end_date      <date> 2016-03-05, 2009-03-05, 2017-09-24, 2004-01-17, ~
```

### Step 1: getCohortSequence

In order to initiate the calculations, the two cohorts tables need to be
combined together. The first cohort is considered to be the index cohort
while the second is the marker.

This process will filter out the individuals who appeared on both tables
according to a user-specified timeGap (default 365, it can also be
changed to Inf if no such restriction is preferred).

The user also has the freedom to specify more than 1 IDs from either
table that can be considered for this process. More concretely, if an
user specifies m IDs from the first cohort and n IDs from the second
cohort, this process will output mn combinations. If IDs are not
specified then all IDs will be considered.

``` r
library(CohortSymmetry)
 
cdm <- CohortSymmetry::getCohortSequence(cdm,
                   indexTable ="cohort_index",
                   markerTable = "cohort_marker",
                   timeGap = Inf)
#> Loading required namespace: testthat
 
cdm$joined_cohorts %>%
  dplyr::glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB 0.7.1 [braventos@Windows 10 x64:R 4.1.2/C:\Users\BRAVEN~1\AppData\Local\Temp\RtmpiUNOTG\file46984e71f0e.duckdb]
#> $ index_id    <int> 1
#> $ marker_id   <int> 1
#> $ subject_id  <int> 2006
#> $ index_date  <date> 2014-01-17
#> $ marker_date <date> 2017-12-31
#> $ first_date  <date> 2014-01-17
#> $ second_date <date> 2017-12-31

cdm$joined_cohorts
#> # Source:   table<joined_cohorts> [1 x 7]
#> # Database: DuckDB 0.7.1 [braventos@Windows 10 x64:R 4.1.2/C:\Users\BRAVEN~1\AppData\Local\Temp\RtmpiUNOTG\file46984e71f0e.duckdb]
#>   index_id marker_id subject_id index_date marker_date first_date second_date
#>      <int>     <int>      <int> <date>     <date>      <date>     <date>     
#> 1        1         1       2006 2014-01-17 2017-12-31  2014-01-17 2017-12-31
```

## References

1.  Pratt N, Chan EW, Choi NK, et al. Prescription sequence symmetry
    analysis: assessing risk, temporality, and consistency for adverse
    drug reactions across datasets in five countries. Pharmacoepidemiol
    Drug Saf. 2015;24(8):858-864.
