% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curb65.fct.R
\name{curb65.fct}
\alias{curb65.fct}
\title{Compute the CRB, CRB65, CURB and CURB65 score.}
\usage{
curb65.fct(
  DID_PROBAND,
  FRM_BEF,
  FRM_B24,
  FRM_DIL_LABORWERTE,
  FRM_RR,
  FRM_BEAT = NULL,
  zp_fabian = "d0",
  event2zeitpunkt_df = progressdatenbankderivate::event2zeitpunkt_table
)
}
\arguments{
\item{DID_PROBAND}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_BEF}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_B24}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_DIL_LABORWERTE}{data.table containing the table with the same
name from the database of the PROGRESS study}

\item{FRM_RR}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_BEAT}{data.table containing the table with the same name from
the database of the PROGRESS study. If it is NULL then the mechanical
ventilation is not taken into account while computing the respiratory
component of CURB65 which is as in the original definition of CURB65.
Otherwise, if FRM_BEAT is non-NULL ventilated patients
(those with the variable PATBEATM == 1 in the table FRM_BEAT) get 1 point.}

\item{zp_fabian}{vector of characters. They must be present in
event2zeitpunkt_table$zp_fabianref.}

\item{event2zeitpunkt_df}{data.table event2zeitpunkt_table (available with
the package).}
}
\value{
a named list with one component: out. It is a data.table with one
row corresponding to one patient, identified by the
PATSTUID. The column crb, crb65, curb, curb65 contain the values of the
scores. Columns PATSTUID and EVENT identify the patient and time point.
Other columns contain data used for computing the components of the CURB65
scores.
If more than 50% of the subscores of each score are avaiable then the
Note: If FRM_BEAT != NULL the calculation of the respiratory component of
the CURB65 score differs from the original definition of CURB65. Patients
who are ventilated are counted as if they their respiratory rate >= 30.
}
\description{
Compute the CRB, CRB65, CURB and CURB65 score.
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR', guess_max = 10e5)
FRM_BEAT <- readxl::read_excel(excel_fn, 'FRM_BEAT', guess_max = 10e5)
data.table::setDT(DID_PROBAND)
data.table::setDT(FRM_BEF)
data.table::setDT(FRM_B24)
data.table::setDT(FRM_DIL_LABORWERTE)
data.table::setDT(FRM_RR)
data.table::setDT(FRM_BEAT)
erg <- curb65.fct(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE, FRM_RR,
FRM_BEAT = NULL, zp_fabian = "d0")
erg
erg_bea <- curb65.fct(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
FRM_RR, FRM_BEAT, zp_fabian = "d0")
erg_bea
}
}
