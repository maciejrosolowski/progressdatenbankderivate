#' Get the value of the Glasgow Coma Scale
#'
#' @param DID_CLIN data.table containing the table DID_CLIN from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the Glasgow Coma Scale, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
#' data.table::setDT(DID_CLIN)
#' getData4gcs(DID_CLIN)
#' }
getData4gcs <- function(DID_CLIN) {
  # due to non-standard evaluation notes in R CMD check
  CLIN_PARAM <- EVENT <- EVENT <- PATSTUID <- WERT <- patstuid <- NULL
  # 56. GCS
  gcs = DID_CLIN[
    CLIN_PARAM=="GCS-SUM",.(patstuid = PATSTUID,
                            event = EVENT,value= as.numeric(WERT))]
  # Hmisc::describe(gcs)
  found_events_gcs = unique(gcs$event  )
  toadd_gcs = dcast.data.table(gcs, patstuid ~ event)
  setnames(toadd_gcs,  as.character(found_events_gcs),
           paste0("gcs_",event2zeitpunkt(found_events_gcs,
                                         returnformat = "zp_fabianref")))

  # stopifnot(nrow(toadd_gcs[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_gcs[, patstuid]) == 0)
  toadd_gcs
}
