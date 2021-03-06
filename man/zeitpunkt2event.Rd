% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zeitpunkt2event.R
\name{zeitpunkt2event}
\alias{zeitpunkt2event}
\title{translate between different labels of the time points}
\usage{
zeitpunkt2event(
  zp_fabian,
  returnformat = c("EVENTref", "zeitpunktref"),
  event2zeitpunkt_df = progressdatenbankderivate::event2zeitpunkt_table
)
}
\arguments{
\item{zp_fabian}{vector of characters. They must be present in
event2zeitpunkt_table$zp_fabianref.}

\item{returnformat}{a character string indicating whether EVENTref or
zeitpunktref should be returned.}

\item{event2zeitpunkt_df}{data.table event2zeitpunkt_table (available with
the package).}
}
\value{
character vector of EVENTref or zeitpunktref
}
\description{
map between the column zp_fabianref and the columns EVENTref or zeitpunktref
in event2zeitpunkt_table. These columns are different labels of the time points
}
\examples{
zeitpunkt2event(zp_fabian = c("auf", "d1", "d5.1"))
}
