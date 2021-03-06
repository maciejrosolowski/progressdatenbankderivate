% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData4sauerst.R
\name{getData4sauerst}
\alias{getData4sauerst}
\title{Get data on the oxygen therapy (supplemental oxygen) (?).}
\usage{
getData4sauerst(FRM_O2A, FRM_O2P)
}
\arguments{
\item{FRM_O2A}{data.table containing the table FRM_O2A from the database
of the PROGRESS study}

\item{FRM_O2P}{data.table containing the table FRM_O2P from the database
of the PROGRESS study}
}
\value{
data.table with the ID of the patient (patstuid), and the
data on the oxygen therapy, in the wide format.
}
\description{
If the column APO2APP in the table FRM_O2A or POXYAPP in the table FRM_O2P
contains a value other than -1 or 1 then oxygen was supplemented.
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
FRM_O2P <- readxl::read_excel(excel_fn, 'FRM_O2P')
data.table::setDT(FRM_O2A)
data.table::setDT(FRM_O2P)
toadd_sauerst <- getData4sauerst(FRM_O2A, FRM_O2P)
toadd_sauerst
}
}
