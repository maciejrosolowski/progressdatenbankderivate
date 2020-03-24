#' Get information on the minimum of the diastolic blood pressure. The
#' minimization is over EVENT within each PATSTUID.
#'
#' @param FRM_RR data.table containing the table FRM_RR from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the maximum of the diastolic blood pressure, in the wide
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
#' getData4diasbp.min(FRM_RR)
#' }
getData4diasbp.min <- function(FRM_RR) {
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- DIASTRR <- event <- patstuid <- value <- NULL
  ##24. Diastolischer Blutdruck (in mmHG)
  diasbp.min  <- FRM_RR[,.(patstuid=PATSTUID, event = EVENT, value=as.numeric(DIASTRR))] %>% unique
  # sysbp.min[allDuplicatedEntries(paste(patstuid, event))]
  diasbp.min2 = diasbp.min[, .(value = min(value, na.rm = T)), .(patstuid,event)]
  diasbp.min2[is.infinite(value), value := NA]
  found_events_diasbp.min = unique(diasbp.min2$event  )
  # Hmisc::describe(diasbp.min2)

  toadd_diasbp.min = dcast.data.table(diasbp.min2, patstuid ~ event)
  setnames(toadd_diasbp.min,  as.character(found_events_diasbp.min), paste0("diasbp.min_",event2zeitpunkt(found_events_diasbp.min, returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_diasbp.min)
  # stopifnot(nrow(toadd_diasbp.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_diasbp.min[, patstuid]) == 0)
  toadd_diasbp.min
}
