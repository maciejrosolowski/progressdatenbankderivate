#' Get information on SNAT.
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE
#' from the database of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on SNAT, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' toadd_snat <- getData4SNAT(FRM_DIL_LABORWERTE)
#' toadd_snat
#' }
getData4SNAT = function(FRM_DIL_LABORWERTE) {
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- UNIT <- VALUE <- NULL
  snat  <- FRM_DIL_LABORWERTE[COL=="SNAT",.(patstuid=PATSTUID, event = EVENT,
                                            UNIT, value=as.numeric(VALUE))] %>% unique
  # snat[allDuplicatedEntries(paste(patstuid, event))]

  found_events_snat = unique(snat$event  )
  # Hmisc::describe(snat)

  toadd_snat = dcast.data.table(snat, patstuid ~ event)
  setnames(toadd_snat,  as.character(found_events_snat),
           paste0("snat_",event2zeitpunkt(found_events_snat,
                                          returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_snat)
  # stopifnot(nrow(toadd_snat[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_snat, by = c("patstuid")) == 0)
  toadd_snat
}
