#' get the oxygenation index (Horowitz index)
#'
#' Values from the rows with CLIN_PARAM == "OxygenIndex-MIN" in the table
#' DID_CLIN are returned.
#'
#' @param DID_CLIN data.table containing the table DID_CLIN from the database
#' of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain the oxygenation index at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_CLIN <- readxl::read_excel(excel_fn, "DID_CLIN")
#' data.table::setDT(DID_CLIN)
#' toadd_oxi.ind <- getData4oxi.ind(DID_CLIN)
#' toadd_oxi.ind
#' }
getData4oxi.ind <- function(DID_CLIN) {
  # due to non-standard evaluation notes in R CMD check
  CLIN_PARAM <- EVENT <- PATSTUID <- WERT <- patstuid <- NULL
  # 57. Oxi-Index
  # DID_CLIN[CLIN_PARAM=="OxygenIndex-MIN"][allDuplicatedEntries(paste(PATSTUID, EVENT))]
  oxi.ind = DID_CLIN[
    CLIN_PARAM=="OxygenIndex-MIN",.(patstuid = PATSTUID, event = EVENT,
                                    value= as.numeric(WERT))]
  # Hmisc::describe(oxi.ind)
  found_events_oxi.ind = unique(oxi.ind$event  )
  toadd_oxi.ind = dcast.data.table(oxi.ind, patstuid ~ event)
  setnames(toadd_oxi.ind,  as.character(found_events_oxi.ind),
           paste0("oxi.ind_",event2zeitpunkt(found_events_oxi.ind,
                                             returnformat = "zp_fabianref")))

  # stopifnot(nrow(toadd_oxi.ind[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_oxi.ind[, patstuid]) == 0)
  toadd_oxi.ind[]
}
