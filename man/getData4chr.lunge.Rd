% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData4chr.lunge.R
\name{getData4chr.lunge}
\alias{getData4chr.lunge}
\title{get data on the presence of the chronic lung disease}
\usage{
getData4chr.lunge(FRM_BAS)
}
\arguments{
\item{FRM_BAS}{data.table containing the table FRM_BAS from the database
of the PROGRESS study}
}
\value{
data.table. Each row corresponds to one patient
and the columns contain information on the presence (1) or absence (0)
of the chronic lung disease.
}
\description{
get data on the presence of the chronic lung disease
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
FRM_BAS <- readxl::read_excel(excel_fn, "FRM_BAS")
data.table::setDT(FRM_BAS)
getData4chr.lunge(FRM_BAS)
}
}
