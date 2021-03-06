% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SCAP.R
\name{SCAP}
\alias{SCAP}
\title{Compute the SCAP score.}
\usage{
SCAP(
  FRM_B24,
  FRM_O2A,
  FRM_RR,
  FRM_BEF,
  FRM_DIL_LABORWERTE,
  DID_CLIN,
  DID_PROBAND,
  FRM_VIS,
  DID_OXYGENIND_SINGLE,
  zp_fabian = "auf_in_d-1_in_d0"
)
}
\arguments{
\item{FRM_B24}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_O2A}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_RR}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_BEF}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_DIL_LABORWERTE}{data.table containing the table with the same
name from the database of the PROGRESS study}

\item{DID_CLIN}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{DID_PROBAND}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{FRM_VIS}{data.table containing the table with the same name from
the database of the PROGRESS study}

\item{DID_OXYGENIND_SINGLE}{data.table containing the table with the same
name from the database of the PROGRESS study}

\item{zp_fabian}{vector of characters. They must be present in
event2zeitpunkt_table$zp_fabianref. Currently, only
zp_fabian = "auf_in_d-1_in_d0" is possible.}
}
\value{
a named list with components: input and out. input is a data.table
in the wide format (one row per patient), containing the data used for
computing the SCAP score. out is a data.table with one row
corresponding to one patient, identified by the
PATSTUID. The column SCAP contains the value of SCAP score. The score is
available, if more than 50% of its subscores, i.e., 5 or more are available.
}
\description{
Compute the SCAP score.
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
FRM_VIS <- readxl::read_excel(excel_fn, 'FRM_VIS')
DID_OXYGENIND_SINGLE <- readxl::read_excel(excel_fn, 'DID_OXYGENIND_SINGLE')
data.table::setDT(FRM_B24)
data.table::setDT(FRM_O2A)
data.table::setDT(FRM_RR)
data.table::setDT(FRM_BEF)
data.table::setDT(FRM_DIL_LABORWERTE)
data.table::setDT(DID_CLIN)
data.table::setDT(DID_PROBAND)
data.table::setDT(FRM_VIS)
data.table::setDT(DID_OXYGENIND_SINGLE)
erg <- SCAP(FRM_B24, FRM_O2A, FRM_RR, FRM_BEF, FRM_DIL_LABORWERTE,
DID_CLIN, DID_PROBAND, FRM_VIS, DID_OXYGENIND_SINGLE,
zp_fabian = "auf_in_d-1_in_d0")
erg
}
}
