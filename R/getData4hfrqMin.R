#' Get information on the mainimal heart rate. The mainimization is over HFREQ from
#' the table FRM_BEF, and HFREQMIN, HFREQMAX from the table FRM_B24
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#' @param FRM_BEF data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the minimal heart rate, in the wide format
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
#' getData4hfrqMin(FRM_B24, FRM_BEF)
#' }
getData4hfrqMin <- function(FRM_B24, FRM_BEF) {
  # 2020-03-04 MRos: there was an additional argument "event" to this function
  # but I removed it.

  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- HFREQMIN <- HFREQMAX <- HFREQ <- value <- patstuid <-
    event <- NULL
  hfrq.min<- FRM_B24[,.(patstuid=PATSTUID, event = EVENT,
                        value=as.numeric(HFREQMIN))] %>% unique
  hfrq.max<- FRM_B24[,.(patstuid=PATSTUID, event = EVENT,
                        value=as.numeric(HFREQMAX))] %>% unique
  hfrq.bef<- FRM_BEF[,.(patstuid=PATSTUID, event = EVENT,
                        value=as.numeric(HFREQ))] %>% unique

  hfrq.all = rbind(hfrq.min, hfrq.max, hfrq.bef)
  # hfrq.all[allDuplicatedEntries(paste(patstuid, event))]
  hfrq.min2 = hfrq.all[, .(value = min(value, na.rm = T)), .(patstuid,event)]
  hfrq.min2[is.infinite(value), value := NA]
  found_events_hfrqMin = unique(hfrq.min2$event  )
  # Hmisc::describe(hfrq.min2)

  # qlist66 = venn4(DAT$patstuid, FRM_BAS$PATSTUID, FRM_BEF$PATSTUID, FRM_B24$PATSTUID)

  toadd_hfrq.min = dcast.data.table(hfrq.min2, patstuid ~ event)
  setnames(toadd_hfrq.min,  as.character(found_events_hfrqMin),
           paste0("hfrq.min_",event2zeitpunkt(found_events_hfrqMin,
                                              returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_hfrq.min)
  # stopifnot(nrow(toadd_hfrq.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_hfrq.min[, patstuid]) == 0)
  toadd_hfrq.min
}
