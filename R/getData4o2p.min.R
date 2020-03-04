#' Get data on the oxygen saturation.
#'
#' A minimum of the oxygen saturation levels, if there were several
#' measurements performed with different methods.
#'
#' @param FRM_O2P data.table containing the table FRM_O2P from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on the oxygen saturation, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_O2P <- readxl::read_excel(excel_fn, 'FRM_O2P')
#' data.table::setDT(FRM_O2P)
#' toadd_o2p.min <- getData4o2p.min(FRM_O2P)
#' toadd_o2p.min
#' }
getData4o2p.min <- function(FRM_O2P) {
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- POXY <- POXYAPP <- value <- patstuid <- event <-
    NULL
  o2p <- FRM_O2P[,.(patstuid=PATSTUID, event = EVENT,
                    value = POXY, POXYAPP)] %>% unique
  o2p.min = o2p[, .(value = min(value, na.rm = T)), .(patstuid,event)]
  o2p.min[is.infinite(value), value := NA]
  found_events_o2p = unique(o2p.min$event  )

  # Hmisc::describe(o2p.min)

  toadd_o2p.min = dcast.data.table(o2p.min, patstuid ~ event)
  setnames(toadd_o2p.min,  as.character(found_events_o2p),
           paste0("o2p.min_",event2zeitpunkt(found_events_o2p,
                                             returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_o2p.min)

  # stopifnot(nrow(toadd_o2p.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_o2p.min, by = c("patstuid")) == 0)
  toadd_o2p.min
}
