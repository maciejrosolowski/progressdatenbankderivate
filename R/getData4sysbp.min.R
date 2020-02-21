#' Get information on the minimum of the systolic blood pressure. The
#' minimization is over EVENT within each PATSTUID.
#'
#' @param FRM_RR data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the maximum of the systolic blood pressure, in the wide
#' format
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
#' data.table::setDT(FRM_RR)
#' getData4sysbp.min(FRM_RR)
#' }
getData4sysbp.min <- function(FRM_RR) {
  PATSTUID <- EVENT <- SYSTRR <- event <- patstuid <- value <- NULL
  ##24. Systolischer Blutdruck (in mmHG)
  sysbp.min  <- FRM_RR[,.(patstuid=PATSTUID, event = EVENT, value=as.numeric(SYSTRR))] %>% unique
  # sysbp.min[allDuplicatedEntries(paste(patstuid, event))]
  sysbp.min2 = sysbp.min[, .(value = min(value, na.rm = T)), .(patstuid,event)]
  sysbp.min2[is.infinite(value), value := NA]
  found_events_sysbp.min = unique(sysbp.min2$event  )
  # Hmisc::describe(sysbp.min2)

  toadd_sysbp.min = dcast.data.table(sysbp.min2, patstuid ~ event)
  setnames(toadd_sysbp.min,  as.character(found_events_sysbp.min), paste0("sysbp.min_",event2zeitpunkt(found_events_sysbp.min, returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_sysbp.min)
  # stopifnot(nrow(toadd_sysbp.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_sysbp.min[, patstuid]) == 0)
  toadd_sysbp.min
}
