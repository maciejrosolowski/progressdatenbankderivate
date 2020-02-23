#' Get data on haematocrit.
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE
#' from the database of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on haematocrit, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' toadd_haemkrt <- getData4haemkrt(FRM_DIL_LABORWERTE)
#' toadd_haemkrt
#' }
getData4haemkrt = function(FRM_DIL_LABORWERTE){
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- UNIT <- VALUE <- value <- NULL
  haemkrt  <- FRM_DIL_LABORWERTE[COL=="HAEMKRIT",
                                 .(patstuid=PATSTUID, event = EVENT,
                                   UNIT, value=as.numeric(VALUE))] %>% unique
  # haemkrt[allDuplicatedEntries(paste(patstuid, event))]

  stopifnot(nrow(haemkrt[value>1])==0)

  found_events_haemkrt = unique(haemkrt$event  )
  # Hmisc::describe(haemkrt)

  toadd_haemkrt = dcast.data.table(haemkrt, patstuid ~ event)
  setnames(toadd_haemkrt,  as.character(found_events_haemkrt),
           paste0("haemkrt_",event2zeitpunkt(found_events_haemkrt,
                                             returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_haemkrt)
  # stopifnot(nrow(toadd_haemkrt[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_haemkrt, by = c("patstuid")) == 0)
  toadd_haemkrt
}
