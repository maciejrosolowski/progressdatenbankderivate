% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractComorbScore.R
\name{extractComorbScore}
\alias{extractComorbScore}
\title{compute the comorbidity score comorbscore}
\usage{
extractComorbScore(FRM_BAS)
}
\arguments{
\item{FRM_BAS}{data.table containing the table FRM_BAS from the database
of the PROGRESS study}
}
\value{
named list with two elements "detailed" and "compact" containing
data.tables with the computed comorbscore and its components.
}
\description{
compute the comorbidity score comorbscore
}
\details{
The comorbscore is the modified Charlson score
(Charlson, Pompei, Ales, and MacKenzie, 1987) and is computed as a sum of the
following points:
\itemize{
\item{} HI (heart failure, Herzinsuffizienz) or SCHRHERZ
(other chronic heart problem): 1 point,
\item{} CEREBROERK (chronic cerebr. disease, Chronische Cerebr. Erkrankung):
 1 point
\item{} CHRNIERE (chronic renal failure, chronische Nierenerkrankung):
2 points
\item{} CHRLEBER (chronic liver disease, chronische Lebererkrankung):
3 points
\item{} DIABETES: 1 point
\item{} CHRATEMLUNG (chronic lung disease,
Chronische Atemwegs/Lungenerkrankung): 1 point
}
}
\examples{
\dontrun{
excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
"PROGRESS/Datenmanagement/Data_freezes/",
"20190320/PROGRESS-freeze_201903_01.xlsx")
FRM_BAS <- readxl::read_excel(excel_fn, "FRM_BAS")
data.table::setDT(FRM_BAS)
extractComorbScore(FRM_BAS)
}
}
