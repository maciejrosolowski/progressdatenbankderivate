% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData4age_sex.R
\name{getData4age.sex}
\alias{getData4age.sex}
\title{get age and sex from the table DID_PROBAND}
\usage{
getData4age.sex(DID_PROBAND)
}
\arguments{
\item{DID_PROBAND}{data.table containing the table DID_PROBAND from the database
of the PROGRESS study}
}
\value{
data.table with the ID of the patient (patstuid), age and sex
}
\description{
get age and sex from the table DID_PROBAND
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
data.table::setDT(DID_PROBAND)
getData4age.sex(DID_PROBAND)
}
}