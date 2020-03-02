#' get data on base excess
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain the data on base excess (Basenueberschuss) at
#' a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, "FRM_B24")
#' data.table::setDT(FRM_B24)
#' toadd_bemin <- getData4bemin(FRM_B24)
#' toadd_bemin
#' }
getData4bemin <- function(FRM_B24) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- BEMIN <- minvalue <- event <- patstuid <- value <-
    NULL
  # 51. BEMIN (Basenueberschuss)
  bemin = FRM_B24[
    ,.(patstuid = PATSTUID, event = EVENT,value= as.numeric(BEMIN))]
  # Hmisc::describe(bemin)

  bemin.min = bemin[, .(minvalue = min(value, na.rm = T)), .(patstuid,event)]
  bemin.min[is.infinite(minvalue), minvalue := NA]

  # Hmisc::describe(thrombo.all.min)
  found_events_bemin = unique(bemin.min$event  )
  toadd_bemin = dcast.data.table(bemin.min, patstuid ~ event,
                                 value.var = "minvalue")
  setnames(toadd_bemin,  as.character(found_events_bemin),
           paste0("bemin_",event2zeitpunkt(found_events_bemin,
                                           returnformat = "zp_fabianref")))

  # stopifnot(nrow(toadd_bemin[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_bemin[, patstuid]) == 0)
  toadd_bemin
}
