#' Get information on the minimum of arterial PH.
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#' @param FRM_O2A data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the minimum of the arterial PH, in the wide format.
#' The minimization is over APH (table FRM_O2A) and APHMIN (table FRM_B24).
#' Small values are bad for the patient.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_O2A)
#' toadd_art.ph <- getData4art.ph(FRM_B24, FRM_O2A)
#' toadd_art.ph
#' }
getData4art.ph <- function(FRM_B24, FRM_O2A) {
  # due to non-standard evaluation notes in R CMD check
  APH <- APHMIN <- EVENT <- PATSTUID <- art.ph.min <- event <- patstuid <-
    value <- NULL
  aph.b24  <- FRM_B24[,.(patstuid=PATSTUID, event = EVENT,
                         value=as.numeric(APHMIN))] %>% unique
  aph.b24$quelle ="aph.b24"
  aph.o2a  <- FRM_O2A[,.(patstuid=PATSTUID, event = EVENT,
                         value=as.numeric(APH))] %>% unique
  aph.o2a$quelle ="aph.o2a"


  art.ph = rbind(aph.b24, aph.o2a)


  art.ph[, art.ph.min := min(value, na.rm = T), .(patstuid,event)]
  art.ph[is.infinite(art.ph.min), art.ph.min := NA]

  art.ph2 = art.ph[,.(patstuid, event, art.ph.min)] %>% unique
  # stopifnot(nrow(art.ph2[allDuplicatedEntries(paste(patstuid, event))])==0)
  stopifnot(anyDuplicated(art.ph2, by = c("patstuid", "event")) == 0)

  found_events_art.ph = unique(art.ph$event)
  # Hmisc::describe(art.ph2) #

  toadd_art.ph = dcast.data.table(art.ph2, patstuid ~ event, value.var = 'art.ph.min')

  setnames(toadd_art.ph,  as.character(found_events_art.ph),
           paste0("art.ph.min_",event2zeitpunkt(found_events_art.ph,
                                                returnformat = "zp_fabianref")))

  # Hmisc::describe(toadd_art.ph.min)
  # stopifnot(nrow(toadd_art.ph[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_art.ph, by = c("patstuid")) == 0)

  toadd_art.ph
}
