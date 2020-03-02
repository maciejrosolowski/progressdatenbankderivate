#' get the data on the mean arterial pressure
#'
#' Extract data with CLIN_PARAM == "MAP-MIN" from the table DID_CLIN and
#' transform it to the wide format.
#'
#' @param DID_CLIN data.table containing the table DID_CLIN from the database
#' of the PROGRESS study
#'
#' @return data.table in the wide format with one row per patient, identified
#' by PATSTUIDs in the column "patstuid" and the values of the mean arterial
#' pressure at each time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_CLIN <- readxl::read_excel(excel_fn, "DID_CLIN")
#' data.table::setDT(DID_CLIN)
#' toadd_map <- getData4map(DID_CLIN)
#' toadd_map
#' }
#'
getData4map <- function(DID_CLIN) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- CLIN_PARAM <- WERT <- patstuid <- NULL
  # 58. MAP
  map = DID_CLIN[
    CLIN_PARAM=="MAP-MIN",.(patstuid = PATSTUID,
                            event = EVENT,value= as.numeric(WERT))]
  # Hmisc::describe(map)
  found_events_map = unique(map$event  )
  toadd_map = dcast.data.table(map, patstuid ~ event)
  setnames(toadd_map,  as.character(found_events_map),
           paste0("map_",event2zeitpunkt(found_events_map,
                                         returnformat = "zp_fabianref")))

  # stopifnot(nrow(toadd_map[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_map[, patstuid]) == 0)
  toadd_map
}
