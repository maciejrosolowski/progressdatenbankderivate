#' get the minimum oxygenation index (Horowitz index) in mmHg
#'
#' Minima of the values from the table DID_OXYGENIND_SINGLE are returned. The
#' minimum is computed for each combination of PATSTUID
#' and EVENT (time point), and is taken over the values of APO2 (table FRM_O2A)
#' and POXY (table FRM_O2P).
#'
#' @param DID_OXYGENIND_SINGLE data.table containing the table so named
#' from the database of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain the oxygenation index at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_OXYGENIND_SINGLE <- readxl::read_excel(excel_fn, "DID_OXYGENIND_SINGLE")
#' data.table::setDT(DID_OXYGENIND_SINGLE)
#' toadd_oxyIndex.min <- getData4oxyIndex(DID_OXYGENIND_SINGLE)
#' toadd_oxyIndex.min
#' }
getData4oxyIndex <- function(DID_OXYGENIND_SINGLE) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- WERT <- event <- patstuid <- value <- NULL
  # 2020-03-05 MRos: conversion from kPa to mmHg
  oxyIndex <- DID_OXYGENIND_SINGLE[,.(patstuid=PATSTUID, event = EVENT,
                                      value = WERT*(1/0.1333224))] %>% unique
  oxyIndex.min = oxyIndex[, .(value = min(value, na.rm = T)),
                          .(patstuid,event)]
  oxyIndex.min[is.infinite(value), value := NA]
  found_events_oxyIndex = unique(oxyIndex.min$event  )

  # Hmisc::describe(oxyIndex.min)

  toadd_oxyIndex.min = dcast.data.table(oxyIndex.min, patstuid ~ event)
  setnames(toadd_oxyIndex.min,  as.character(found_events_oxyIndex),
           paste0("oxyIndex.min_",
                  event2zeitpunkt(found_events_oxyIndex,
                                  returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_oxyIndex.min)

  # stopifnot(nrow(toadd_oxyIndex.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_oxyIndex.min[, patstuid]) == 0)
  toadd_oxyIndex.min[]
}
