
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

Read the data exported from the database of the PROGRESS project

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
DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
FRM_KAT <- readxl::read_excel(excel_fn, 'FRM_KAT')
FRM_O2P <- readxl::read_excel(excel_fn, 'FRM_O2P')
FRM_BEAT <- readxl::read_excel(excel_fn, 'FRM_BEAT', guess_max = 10e5)
DID_OXYGENIND_SINGLE <- readxl::read_excel(excel_fn, 'DID_OXYGENIND_SINGLE')
data.table::setDT(DID_PROBAND)
data.table::setDT(FRM_BAS)
data.table::setDT(FRM_BEF)
data.table::setDT(FRM_B24)
data.table::setDT(FRM_RR)
data.table::setDT(FRM_O2A)
data.table::setDT(FRM_DIL_LABORWERTE)
data.table::setDT(FRM_VIS)
data.table::setDT(DID_CLIN)
data.table::setDT(FRM_KAT)
data.table::setDT(FRM_O2P)
data.table::setDT(FRM_BEAT)
data.table::setDT(DID_OXYGENIND_SINGLE)
```

### PSI

This is an example which shows how to compute the Pneumonia Severity
Index
(PSI):

``` r
# suppress warnings about no non-missing values while computing min or max
# by PATSTUID and EVENT.
suppressWarnings(
  psi_d0 <- psi.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24, FRM_RR, FRM_O2A, 
                    FRM_DIL_LABORWERTE,FRM_VIS, zp_fabian = "d0")$out[
                      , .(PATSTUID, EVENT, psi)]
)
psi_d0
#>       PATSTUID EVENT psi
#>    1:     5635     3   1
#>    2:     5656     3   3
#>    3:     5663     3   4
#>    4:     5674     3   4
#>    5:     5681     3   3
#>   ---                   
#> 2209:   238909     3   2
#> 2210:   240062     3   2
#> 2211:   242021     3   2
#> 2212:   244008     3   2
#> 2213:     9435     3   2
```

### SIRS

``` r
suppressWarnings(
  sirs_d0 <- sirs.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                      DID_CLIN, FRM_RR, FRM_KAT, zp_fabian = "d0")$out[
                        , .(PATSTUID, EVENT, sirs = infec.septic.servsept,
                   schock = septischer.schock)]
)
sirs_d0
#>       PATSTUID EVENT sirs schock
#>    1:     5635     3    2  FALSE
#>    2:     5656     3    2  FALSE
#>    3:     5663     3    1  FALSE
#>    4:     5674     3    1  FALSE
#>    5:     5681     3    2  FALSE
#>   ---                           
#> 2209:   238909     3    1  FALSE
#> 2210:   240062     3    1  FALSE
#> 2211:   242021     3    1  FALSE
#> 2212:   244008     3    1  FALSE
#> 2213:     9435     3    1  FALSE
```

### quickSOFA

``` r
suppressWarnings(
  qsofa <- quickSOFA(FRM_RR, FRM_B24, FRM_BEF, DID_CLIN,
                     zp_fabian = "auf_in_d0")$out
)
qsofa[, .(PATSTUID, EVENT, qSOFA)]
#>       PATSTUID EVENT qSOFA
#>    1:     1564    31     1
#>    2:     1591    31     0
#>    3:     1680    31     1
#>    4:     1740    31     2
#>    5:     1790    31     0
#>   ---                     
#> 2209:    88954    31     0
#> 2210:   184339    31     0
#> 2211:   231590    31     0
#> 2212:   231626    31     0
#> 2213:   242021    31     1
```

### Halm

``` r
suppressWarnings(
  halm <- HalmScore(FRM_B24, FRM_BEF, FRM_RR, FRM_O2A, FRM_O2P, FRM_BEAT,
                    DID_CLIN, zp_fabian = "auf_in_d-1_in_d0")$out[
                      , .(PATSTUID, EVENT, halm)
                    ]
)
halm
#>       PATSTUID EVENT halm
#>    1:     1564   321    1
#>    2:     1586   321    0
#>    3:     1591   321    1
#>    4:     1680   321    3
#>    5:     1740   321    2
#>   ---                    
#> 2209:   243434   321    3
#> 2210:   243708   321    3
#> 2211:   243719   321    4
#> 2212:   244008   321    1
#> 2213:     9435   321    0
```

### SCAP

``` r
suppressWarnings(
  scap <- SCAP(FRM_B24, FRM_O2A, FRM_RR, FRM_BEF, FRM_DIL_LABORWERTE, DID_CLIN,
               DID_PROBAND, FRM_VIS, DID_OXYGENIND_SINGLE,
               zp_fabian = "auf_in_d-1_in_d0")$out[, .(PATSTUID, EVENT, SCAP)]
)
scap
#>       PATSTUID EVENT SCAP
#>    1:     1564   321    0
#>    2:     1586   321    0
#>    3:     1591   321    0
#>    4:     1680   321    5
#>    5:     1740   321   21
#>   ---                    
#> 2209:   243434   321    6
#> 2210:   243708   321   11
#> 2211:   243719   321   11
#> 2212:   244008   321    6
#> 2213:     9435   321    0
```

### smartCOP

``` r
suppressWarnings(
  smart_cop <- smartCOP(FRM_RR, FRM_BEF, FRM_VIS, FRM_DIL_LABORWERTE, FRM_B24,
                        DID_PROBAND, DID_CLIN, DID_OXYGENIND_SINGLE,
                        FRM_O2A, zp_fabian = "auf_in_d-1_in_d0")$out
)
smart_cop
#>       PATSTUID EVENT smartCOP
#>    1:     1564   321        0
#>    2:     1591   321        2
#>    3:     1680   321        1
#>    4:     1740   321        3
#>    5:     1790   321        2
#>   ---                        
#> 2209:    88954   321        0
#> 2210:   184339   321        0
#> 2211:   231590   321        0
#> 2212:   231626   321        0
#> 2213:   242021   321        1
```

### CRB, CRB65, CURB, CURB65

``` r
suppressWarnings(
  curb65 <- curb65.fct(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE, 
                       FRM_RR, FRM_BEAT = NULL, zp_fabian = "d0")$out[,
                         .(PATSTUID, EVENT, crb, crb65, curb, curb65)]
)
curb65
#>       PATSTUID EVENT crb crb65 curb curb65
#>    1:     5635     3   0     0    0      0
#>    2:     5656     3   0     1    0      1
#>    3:     5663     3   1     2    1      2
#>    4:     5674     3   1     2    2      3
#>    5:     5681     3   0     1    0      1
#>   ---                                     
#> 2209:   238909     3   0     0    0      0
#> 2210:   240062     3   1     1    1      1
#> 2211:   242021     3   0     0    0      0
#> 2212:   244008     3   1     1    1      1
#> 2213:     9435     3   0     0    0      0
```

## How to compute the scores based on values of the input parameters

### PSI

``` r
psi <- psi_simple(
  age = 68,
  verwirrt = 1,
  hfrq.max = 130,
  afrq.max = 35,
  sysbp.min = 80,
  temp.min = 34,
  temp.max = 35,
  tumor = 1,
  herz = 1,
  cerebro = 1,
  renal = 1,
  liver = 1,
  gender = 1,
  nurse.home = 1,
  art.ph.min = 7.2,
  bun = 10,
  snat = 120,
  gluk = 16,
  haemkrt = 0.2,
  apo2.min = 50,
  pleu_erg = 1)
psi
#> [1] 5
```

An example with vectors as arguments in the function call

``` r
psi <- psi_simple(
  age = c(68, 61),
  verwirrt = c(1, 0),
  hfrq.max = c(130, 125),
  afrq.max = c(35, 26),
  sysbp.min = c(80, 98),
  temp.min = c(34, 36),
  temp.max = c(35, 37),
  tumor = c(1, 0),
  herz = c(1, 0),
  cerebro = c(1, 0),
  renal = c(1, 0),
  liver = c(1, 0),
  gender = c(1, 0),
  nurse.home = c(1, 0),
  art.ph.min = c(7.2, 7.5),
  bun = c(10, 11),
  snat = c(120, 134),
  gluk = c(16, 9),
  haemkrt = c(0.2, 0.25),
  apo2.min = c(50, 52),
  pleu_erg = c(1, 0))
psi
#> [1] 5 4
```

### SIRS

``` r
sirs <- sirs_simple(
  temp.min = 34,
  temp.max = 35,
  hfrq.max = 104,
  afrq.max = 20,
  pco2 = 4.1,
  leuko_min = 7800,
  leuko_max = 7900,
  stkern.neutro = 395,
  smkern.neutro = 4424,
  verwirrt = 1,
  thrombo_min = 216000,
  thrombo.daybefore = 205000,
  oxi.ind = 43.81,
  chr.lunge = 1,
  diur = 3120,
  gewicht = 70,
  bemin = 3.4,
  sysbp.min = 95,
  map = 10.44,
  kate = FALSE)
sirs
#> $infec.septic.servsept
#> [1] 3
#> 
#> $shock
#> [1] FALSE
```

### Halm

``` r
halm <- HalmScore_simple(
  hfrq.max = 104,
  sysbp.min = 95,
  afrq.max = 19,
  o2p.min = 90,
  apo2.min = 50,
  bea = 1,
  sauerst = 0,
  temp.max = 38,
  verwirrt = 0,
  gcs = 15)
halm
#> [1] 3
```
