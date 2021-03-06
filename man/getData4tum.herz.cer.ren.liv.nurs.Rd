% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData4tum.herz.cer.ren.liv.nurs.R
\name{getData4tum.herz.cer.ren.liv.nurs}
\alias{getData4tum.herz.cer.ren.liv.nurs}
\title{Get data on TUMOR, HI (heart insufficiency), CEREBROERK, CHRNIERE,
CHRLEBER and nurse.home.}
\usage{
getData4tum.herz.cer.ren.liv.nurs(FRM_BAS, change_names = TRUE)
}
\arguments{
\item{FRM_BAS}{data.table containing the table FRM_BAS
from the database of the PROGRESS study}

\item{change_names}{a logical value. If TRUE, then the names of the patient
characteristics will be changed. This is expected by the functions which
computed the severity scores.}
}
\value{
data.table with the ID of the patient (patstuid), and the
information on tumor status (TUMOR), heart insufficiency (HI),
CEREBROERK, chronic renal disease (CHRNIERE), chronic liver disease
(CHRLEBER), and the data on whether the patient is in a nursing home,
in the wide format.
}
\description{
Get data on TUMOR, HI (heart insufficiency), CEREBROERK, CHRNIERE,
CHRLEBER and nurse.home.
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
FRM_BAS <- readxl::read_excel(excel_fn, 'FRM_BAS')
data.table::setDT(FRM_BAS)
toadd_tum.herz.cer.ren.liv.nurs <- getData4tum.herz.cer.ren.liv.nurs(FRM_BAS)
toadd_tum.herz.cer.ren.liv.nurs[]
toadd_tum.herz.cer.ren.liv.nurs <-
getData4tum.herz.cer.ren.liv.nurs(FRM_BAS, change_names = FALSE)
toadd_tum.herz.cer.ren.liv.nurs[]
}
}
