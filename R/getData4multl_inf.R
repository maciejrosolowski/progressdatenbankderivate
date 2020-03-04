#' Get data on the presence of multilobar infiltrates.
#'
#' @param FRM_BEF data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#' @param FRM_VIS data.table containing the table FRM_VIS from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on the presence of multilobar infiltrates, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' FRM_VIS <- readxl::read_excel(excel_fn, 'FRM_VIS')
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_VIS)
#' toadd_multl_inf <- getData4multl_inf(FRM_BEF, FRM_VIS)
#' toadd_multl_inf
#' }
getData4multl_inf <- function(FRM_BEF, FRM_VIS) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- RTHORMINF <- XTHORINF <- event <- patstuid <- quelle <-
    value <- NULL
  # 36. Sauerstoffzugabe (falls in APO2APP oder in POXYAPP etwas ausser
  # -1 udn 1 steht, wird Sauerstoff zugefuehrt)
  minf1 <- FRM_BEF[,.(patstuid=PATSTUID, event = EVENT,
                      value = RTHORMINF)] %>% unique
  minf1$quelle = "FRM_BEF"
  minf2 <- FRM_VIS[,.(patstuid=PATSTUID, event = EVENT,
                      value = XTHORINF)] %>% unique
  minf2$quelle = "FRM_VIS"

  minf = rbind(minf1, minf2)
  minf[value %in% c(-1,99), value:= NA]

  # Hmisc::describe(minf)
  minf[,minf := ifelse(unique(event) %in% c(1,3), value[quelle =="FRM_BEF"],
                       value[quelle =="FRM_VIS"]), .(patstuid,event)]

  found_events_minf = unique(minf$event  )
  # Hmisc::describe(minf)

  # 2020-03-04 MRos: the cast is done here also by minf but this does not
  # change anything in this case because there is only one row per patstuid
  # and event.
  toadd_multl_inf = dcast.data.table(unique(minf[
    ,.(patstuid, event, minf)]), patstuid ~ event,value.var = "minf",
    fill = NA)
  setnames(toadd_multl_inf,  as.character(found_events_minf),
           paste0("multl_inf_",event2zeitpunkt(found_events_minf,
                                               returnformat = "zp_fabianref")))
  toadd_multl_inf
}
