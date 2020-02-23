#' Get data on the minimum of the part pressure of arterial O2.
#'
#' @param FRM_O2A data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on the part pressure of arterial O2 (in mmHg), in the wide format.
#' Small values are bad for the patient.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' data.table::setDT(FRM_O2A)
#' toadd_apo2.min <- getData4apo2.min(FRM_O2A)
#' toadd_apo2.min
#' }
getData4apo2.min <- function(FRM_O2A) {
  # due to non-standard evaluation notes in R CMD check
  APO2 <- APO2EINH <- EVENT <- PATSTUID <- event <- patstuid <- value <- NULL
  apo2.o2a  <- FRM_O2A[,.(patstuid=PATSTUID, event = EVENT,APO2EINH,
                          value=as.numeric(APO2))] %>% unique
  apo2.o2a[,.N,APO2EINH]
  stopifnot(all(apo2.o2a$APO2EINH %in% c(-1,4,5)))
  # 2020-02-23 MRos: conversion to mmHg
  apo2.o2a[APO2EINH==4, value := value * 7.50061505]
  # katrin hatte bei HalmScore 1/0.133224 = 7.506155
  apo2.o2a[APO2EINH==-1, value := NA]

  # apo2.o2a[allDuplicatedEntries(paste(patstuid, event))]
  # Hmisc::describe(apo2.o2a)
  apo2.min = apo2.o2a[, .(value = min(value, na.rm = T)), .(patstuid,event)]
  apo2.min[is.infinite(value), value := NA]
  found_events_apo2.min = unique(apo2.min$event  )
  # Hmisc::describe(apo2.min)

  toadd_apo2.min = dcast.data.table(apo2.min, patstuid ~ event)
  setnames(toadd_apo2.min,  as.character(found_events_apo2.min),
           paste0("apo2.min_",event2zeitpunkt(found_events_apo2.min,
                                              returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_apo2.min)

  # stopifnot(nrow(toadd_apo2.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_apo2.min, by = c("patstuid")) == 0)
  toadd_apo2.min
}
