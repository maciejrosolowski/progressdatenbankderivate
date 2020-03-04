#' Get data on the oxygen therapy (supplemental oxygen) (?).
#'
#' If the column APO2APP in the table FRM_O2A or POXYAPP in the table FRM_O2P
#' contains a value other than -1 or 1 then oxygen was supplemented.
#'
#' @param FRM_O2A data.table containing the table FRM_O2A from the database
#' of the PROGRESS study
#' @param FRM_O2P data.table containing the table FRM_O2P from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on the oxygen therapy, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' FRM_O2P <- readxl::read_excel(excel_fn, 'FRM_O2P')
#' data.table::setDT(FRM_O2A)
#' data.table::setDT(FRM_O2P)
#' toadd_sauerst <- getData4sauerst(FRM_O2A, FRM_O2P)
#' toadd_sauerst
#' }
getData4sauerst <- function(FRM_O2A, FRM_O2P) {
  # due to non-standard evaluation notes in R CMD check
  APO2APP <- EVENT <- PATSTUID <- POXYAPP <- event <- patstuid <- value <-
    NULL
  # 36. Sauerstoffzugabe (falls in APO2APP oder in POXYAPP etwas ausser
  # -1 und 1 steht, wird Sauerstoff zugefuehrt)
  sauerst1 <- FRM_O2A[
    ,.(patstuid=PATSTUID, event = EVENT,value = APO2APP)] %>% unique
  sauerst2 <- FRM_O2P[
    ,.(patstuid=PATSTUID, event = EVENT,value = POXYAPP)] %>% unique

  sauerst = rbind(sauerst1, sauerst2)
  # Hmisc::describe(sauerst)
  sauerst[,sauerst := ifelse(any(value %in%2:7), 1,
                             ifelse(any(value==-1), NA_real_, 0)),
          .(patstuid, event)]

  found_events_sauerst = unique(sauerst$event  )
  # Hmisc::describe(apo2.min)

  toadd_sauerst = dcast.data.table(unique(sauerst[
    ,.(patstuid, event, sauerst)]), patstuid ~ event,
    value.var = "sauerst", fill = 0)
  setnames(toadd_sauerst,  as.character(found_events_sauerst),
           paste0("sauerst_",event2zeitpunkt(found_events_sauerst,
                                             returnformat = "zp_fabianref")))
  toadd_sauerst
}
