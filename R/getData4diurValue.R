#' get data on diuresis
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain the data on diuresis at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, "FRM_B24")
#' data.table::setDT(FRM_B24)
#' toadd_diurValue <- getData4diurValue(FRM_B24)
#' toadd_diurValue
#' }
getData4diurValue <- function(FRM_B24) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- DIURESE <- minvalue <- event <- patstuid <- value <-
    NULL
  # 50. Diurese (zahlenwerte)
  # FRM_B24[allDuplicatedEntries(paste(PATSTUID, EVENT))]
  diurValue = FRM_B24[
    ,.(patstuid = PATSTUID, event = EVENT,value= as.numeric(DIURESE))]
  # Hmisc::describe(diurValue)

  diurValue.min = diurValue[
    , .(minvalue = min(value, na.rm = T)), .(patstuid,event)]
  diurValue.min[is.infinite(minvalue), minvalue := NA]

  # Hmisc::describe(thrombo.all.min)
  found_events_diurValue = unique(diurValue.min$event  )
  toadd_diurValue = dcast.data.table(diurValue.min, patstuid ~ event,
                                     value.var = "minvalue")
  setnames(toadd_diurValue,  as.character(found_events_diurValue),
           paste0("diur_",event2zeitpunkt(found_events_diurValue,
                                          returnformat = "zp_fabianref")))

  # stopifnot(nrow(toadd_diurValue[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_diurValue[, patstuid]) == 0)
  toadd_diurValue
}
