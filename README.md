
<!-- README.md is generated from README.Rmd. Please edit that file -->

# progressdatenbankderivate

<!-- badges: start -->

<!-- badges: end -->

The goal of progressdatenbankderivate is to extract data from an excel
file containing an export from the database of the PROGRESS project and
to compute several pneumonia scores based on this data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maciejrosolowski/progressdatenbankderivate")
```

## Example

This is an example which shows how to compute the Pneumonia Severity
Index (PSI):

``` r
library(readxl)
library(data.table)
library(progressdatenbankderivate)
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/", 
                   "PROGRESS/Datenmanagement/Data_freezes/", 
                   "20190320/PROGRESS-freeze_201903_01.xlsx")
DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
FRM_BAS <- readxl::read_excel(excel_fn, 'FRM_BAS')
FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR', guess_max = 10e5)
FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
FRM_VIS <- readxl::read_excel(excel_fn, 'FRM_VIS')
data.table::setDT(DID_PROBAND)
data.table::setDT(FRM_BAS)
data.table::setDT(FRM_BEF)
data.table::setDT(FRM_B24)
data.table::setDT(FRM_RR)
data.table::setDT(FRM_O2A)
data.table::setDT(FRM_DIL_LABORWERTE)
data.table::setDT(FRM_VIS)
# suppress warnings about no non-missing values while computing min or max
# by PATSTUID and EVENT.
suppressWarnings(
  erg_d0 <- psi.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24, FRM_RR, FRM_O2A, 
                  FRM_DIL_LABORWERTE,FRM_VIS, zp_fabian = "d0")
)
erg_d0$out
#>       PATSTUID event psi.class psi.class.filt vollstaendig.von.20
#>    1:     5635     3         1             NA                  17
#>    2:     5656     3         3             NA                  17
#>    3:     5663     3         4             NA                   8
#>    4:     5674     3         4             NA                  19
#>    5:     5681     3         3             NA                   9
#>   ---                                                            
#> 2209:   238909     3         2             NA                   1
#> 2210:   240062     3         2             NA                   1
#> 2211:   242021     3         2             NA                   0
#> 2212:   244008     3         2             NA                   1
#> 2213:     9435     3         2             NA                   1
# erg_d1 <- psi.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24, FRM_RR, FRM_O2A,
# FRM_DIL_LABORWERTE,FRM_VIS, zp_fabian = "d1")
# erg_d1
```
