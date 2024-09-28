#' Get data on the partial pressure of carbon dioxide PCO2MIN.
#'
#' The fucntion gets the value of PCO2MIN and returns its maximum for each
#' patient and EVENT == 3 (inclusion) in kPa.
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on the partial pressure of CO2 (in kPa), in the wide format
#' (at EVENT == 3 only).
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' data.table::setDT(FRM_B24)
#' toadd_pco2.max <- getData4pco2(FRM_B24)
#' toadd_pco2.max
#' }
getData4pco2 <- function(FRM_B24) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- PCO2EINH <- PCO2MIN <- patstuid <- event <- value <-
    NULL
  # 33 PCO2 (aus B24; in O2A steht zwar auch eine Spalte PCO2 drin, aber ohne Eintraege)
  pco2  <- FRM_B24[,.(patstuid=PATSTUID,
                      event = EVENT, PCO2EINH,value=as.numeric(PCO2MIN))] %>% unique
  stopifnot(all(pco2$PCO2EINH %in% c(-1,4,5)))
  # 2020-02-28 MRos: conversion from mmHg to kPa
  # pco2[PCO2EINH==5 , value := value * 0.133322400007716]
  # 2024-09-28 MRos: source: https://www.convertunits.com/from/mm%20Hg/to/kPa
  pco2[PCO2EINH==5 , value := value * 0.133322387415]
  # pco2[allDuplicatedEntries(paste(patstuid, event))]
  pco2.max2 = pco2[event ==3, .(value = max(value, na.rm = T)), .(patstuid,event)]
  pco2.max2[is.infinite(value), value := NA]
  found_events_pco2 = unique(pco2.max2$event  )
  # Hmisc::describe(pco2.max2)

  toadd_pco2.max = dcast.data.table(pco2.max2, patstuid ~ event)
  setnames(toadd_pco2.max,  as.character(found_events_pco2),
           paste0("pco2_", event2zeitpunkt(found_events_pco2,
                                           returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_pco2.max)
  # stopifnot(nrow(toadd_pco2.max[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_pco2.max[, patstuid]) == 0)
  toadd_pco2.max
}
