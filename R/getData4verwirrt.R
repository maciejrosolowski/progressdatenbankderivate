#' get information on whether the patient is confused (verwirrt) from the table FRM_BEF
#'
#' @param FRM_BEF data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on whether the patient is confused, in the wide format
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' data.table::setDT(FRM_BEF)
#' toadd_verwirrt <- getData4verwirrt(FRM_BEF)
#' toadd_verwirrt[]
#' }
getData4verwirrt <- function(FRM_BEF) {
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT<- VERWIRRTH <- value <- patstuid <- NULL
  # FRM_BEF[allDuplicatedEntries(paste(PATSTUID, EVENT))]
  toadd_verwirrt = FRM_BEF[,.(patstuid = PATSTUID, event = EVENT,
                              value= as.numeric(VERWIRRTH))]
  toadd_verwirrt[value %in% c(-1,99) , value := NA]
  found_events_verwirrt = unique(toadd_verwirrt$event  )
  toadd_verwirrt = dcast.data.table(toadd_verwirrt, patstuid ~ event)
  setnames(toadd_verwirrt,  as.character(found_events_verwirrt),
           paste0("verwirrt_",event2zeitpunkt(found_events_verwirrt,
                                              returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_verwirrt)
  # stopifnot(nrow(toadd_verwirrt[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_verwirrt[, patstuid]) == 0)
  toadd_verwirrt[]
}
