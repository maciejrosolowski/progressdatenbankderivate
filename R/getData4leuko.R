#' get the leukocyte count from the table FRM_LABORWERTE
#'
#' The values with COL == "LEUKO" in the table FRM_LABORWERTE are returned
#' with the suffix "_dbb". The maximum over the values with COL in "LEUKO",
#' "LEUKOMIN" or "LEUKOMAX" within each combination of patient and event
#' is returned with the suffix "_max". Similarly, the minimum over these
#' values is returned with the suffix "_min".
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE
#' from the database of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain leukocyte count (dbb, min, max)
#' at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' leuko_dat <- getData4leuko(FRM_DIL_LABORWERTE)
#' leuko_dat
#' }
getData4leuko <- function(FRM_DIL_LABORWERTE) {
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- DIFFBB <- UNIT <- VALUE <- event <- maxvalue <-
    minvalue <- patstuid <- value <- NULL
  leuko <- FRM_DIL_LABORWERTE[
    COL=="LEUKO", .(patstuid=PATSTUID, event=EVENT,UNIT ,DIFFBB,
                    value= as.numeric(VALUE), quelle = "leuko")] %>% unique
  leuko.min <- FRM_DIL_LABORWERTE[
    COL=="LEUKOMIN", .(patstuid=PATSTUID, event=EVENT,UNIT ,DIFFBB,
                       value= as.numeric(VALUE), quelle = "leuko.min")
    ] %>% unique
  leuko.max <- FRM_DIL_LABORWERTE[
    COL=="LEUKOMAX", .(patstuid=PATSTUID, event=EVENT,UNIT ,DIFFBB,
                       value= as.numeric(VALUE), quelle = "leuko.max")
    ] %>% unique

  # library(assertr)
  # leuko[allDuplicatedEntries(paste(patstuid, event))] %>% verify(nrow(.)==0)
  # leuko.min[allDuplicatedEntries(paste(patstuid, event))]%>% verify(nrow(.)==0)
  # leuko.max[allDuplicatedEntries(paste(patstuid, event))]%>% verify(nrow(.)==0)
  stopifnot(anyDuplicated(leuko, by = c("patstuid", "event")) == 0)
  stopifnot(anyDuplicated(leuko.min, by = c("patstuid", "event")) == 0)
  stopifnot(anyDuplicated(leuko.max, by = c("patstuid", "event")) == 0)

  leuko.all = rbind(leuko, leuko.min, leuko.max)
  # Hmisc::describe(leuko.all)

  found_events_leuko = unique(leuko$event  )

  toadd_leuko = dcast.data.table(leuko, patstuid ~ event)
  setnames(toadd_leuko,  as.character(found_events_leuko),
           paste0("leuko_dbb_",event2zeitpunkt(found_events_leuko,
                                               returnformat = "zp_fabianref")))
  # stopifnot(nrow(toadd_leuko[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_leuko, by = c("patstuid")) == 0)
  # toadd_leuko

  leuko.all.min = leuko.all[
    , .(minvalue = min(value, na.rm = T)), .(patstuid,event)]
  leuko.all.min[is.infinite(minvalue), minvalue := NA]
  found_events_leuko.min = unique(leuko.all.min$event  )
  # Hmisc::describe(leuko.all.min)
  toadd_leuko.all.min = dcast.data.table(leuko.all.min, patstuid ~ event,
                                         value.var = "minvalue")
  setnames(toadd_leuko.all.min,  as.character(found_events_leuko.min),
           paste0("leuko_min_",event2zeitpunkt(found_events_leuko.min,
                                               returnformat = "zp_fabianref")))
  # stopifnot(nrow(toadd_leuko.all.min[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_leuko.all.min, by = c("patstuid")) == 0)
  # toadd_leuko.all.min

  leuko.all.max = leuko.all[
    , .(maxvalue = max(value, na.rm = T)), .(patstuid,event)]
  leuko.all.max[is.infinite(maxvalue), maxvalue := NA]
  found_events_leuko.max = unique(leuko.all.max$event  )
  # Hmisc::describe(leuko.all.max)
  toadd_leuko.all.max = dcast.data.table(leuko.all.max, patstuid ~ event,
                                         value.var = "maxvalue")
  setnames(toadd_leuko.all.max,  as.character(found_events_leuko.max),
           paste0("leuko_max_",event2zeitpunkt(found_events_leuko.max,
                                               returnformat = "zp_fabianref")))
  # stopifnot(nrow(toadd_leuko.all.max[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_leuko.all.max, by = c("patstuid")) == 0)
  # toadd_leuko.all.max

  dat = merge(toadd_leuko, toadd_leuko.all.min, by = 'patstuid', all = T,sort = F)
  dat = merge(dat, toadd_leuko.all.max, by = 'patstuid', all = T,sort = F)
  # stopifnot(nrow(dat[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(dat, by = c("patstuid")) == 0)
  dat[]
}
