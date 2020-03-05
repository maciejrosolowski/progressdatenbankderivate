#' Get the albumin levels from the database of the PROGRESS study
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table so named from
#' the database of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' albumin level, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' toadd_alb <- getData4albumin(FRM_DIL_LABORWERTE)
#' toadd_alb
#' }
getData4albumin <- function(FRM_DIL_LABORWERTE) {
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- UNIT <- VALUE <- patstuid <- NULL
  # 56. alb
  alb = FRM_DIL_LABORWERTE[
    COL=="ALB_ZLAB",.(patstuid = PATSTUID, event = EVENT,UNIT,
                      value= as.numeric(VALUE))]
  stopifnot(all(alb$UNIT=="g/l"))
  # Hmisc::describe(alb)
  found_events_alb = unique(alb$event  )
  toadd_alb = dcast.data.table(alb, patstuid ~ event)
  setnames(toadd_alb,  as.character(found_events_alb),
           paste0("alb_",event2zeitpunkt(found_events_alb,
                                         returnformat = "zp_fabianref")))

  # stopifnot(nrow(toadd_alb[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_alb[, patstuid]) == 0)
  toadd_alb
}
