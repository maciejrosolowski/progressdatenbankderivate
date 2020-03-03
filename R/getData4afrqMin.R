#' Get information on the minimal beathing rate (respiratory frequency). The
#' minimization is over AFREQ from the table FRM_BEF, and AFREQMIN, AFREQMAX from
#' the table FRM_B24
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#' @param FRM_BEF data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the minimal breathing rate, in the wide format
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_BEF)
#' getData4afrqMin(FRM_B24, FRM_BEF)
#' }
getData4afrqMin <- function(FRM_B24, FRM_BEF) {
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- AFREQMIN <- AFREQMAX <- AFREQ <- value <- patstuid <-
    event <- NULL
  afrq.min<- FRM_B24[,.(patstuid=PATSTUID, event = EVENT,
                        value=as.numeric(AFREQMIN))] %>% unique
  afrq.max<- FRM_B24[,.(patstuid=PATSTUID,
                        event = EVENT, value=as.numeric(AFREQMAX))] %>% unique
  afrq.bef<- FRM_BEF[,.(patstuid=PATSTUID,
                        event = EVENT, value=as.numeric(AFREQ))] %>% unique

  afrq.all= rbind(afrq.min, afrq.max, afrq.bef)

  # afrq.all[allDuplicatedEntries(paste(patstuid, event))]
  afrq.min2 = afrq.all[, .(value = min(value, na.rm = T)), .(patstuid,event)]
  afrq.min2[is.infinite(value), value := NA]
  found_events_afrqMin = unique(afrq.min2$event  )
  # Hmisc::describe(afrq.min2)

  toadd_afrq.min = dcast.data.table(afrq.min2, patstuid ~ event)
  setnames(toadd_afrq.min,  as.character(found_events_afrqMin),
           paste0("afrq.min_",event2zeitpunkt(found_events_afrqMin,
                                              returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_afrq.min)
  # stopifnot(nrow(toadd_afrq.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_afrq.min[, patstuid]) == 0)
  toadd_afrq.min
}
