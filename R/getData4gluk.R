#' get age and sex from the table DID_PROBAND
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE from the database
#' of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain blood glucose measurements at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' getData4gluk(FRM_DIL_LABORWERTE)
#' }
getData4gluk = function(FRM_DIL_LABORWERTE){
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- UNIT <- VALUE <- patstuid <- NULL
  gluk  <- FRM_DIL_LABORWERTE[COL=="BGLUK",.(patstuid=PATSTUID,
                                             event = EVENT,UNIT,
                                             value=as.numeric(VALUE))] %>% unique
  # gluk[allDuplicatedEntries(paste(patstuid, event))]

  found_events_gluk = unique(gluk$event  )
  # Hmisc::describe(gluk)

  toadd_gluk = dcast.data.table(gluk, patstuid ~ event)
  setnames(toadd_gluk,  as.character(found_events_gluk),
           paste0("gluk_",event2zeitpunkt(found_events_gluk,
                                          returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_gluk)
  # stopifnot(nrow(toadd_gluk[allDuplicatedEntries(patstuid)])==0)
  # alternative code, not using the R package toolboxH
  stopifnot(anyDuplicated(toadd_gluk[, patstuid]) == 0)
  toadd_gluk
}
