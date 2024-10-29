#' Get information on the body temperature.
#'
#' @param FRM_B24 data.table containing the table FRM_B24 from the database
#' of the PROGRESS study
#' @param FRM_BEF data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the body temperature, in the wide format
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_BEF)
#' toadd_temp <- getData4temp(FRM_BEF, FRM_B24)
#' toadd_temp
#' }
getData4temp <- function(FRM_BEF, FRM_B24) {
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- KTEMPO <- KTEMP <- KTEMPMAX <- KTEMPMIN <- event <-
    maxvalue <- maxvalue_ort.bek <- maxvalue_ort.unbek <- minvalue <-
    minvalue_ort.bek <- minvalue_ort.unbek <- ort <- ort.unbek <- patstuid <-
    showNA <- value <- NULL
  temp.bef  <- FRM_BEF[,.(patstuid=PATSTUID, event = EVENT, ort = KTEMPO,value=as.numeric(KTEMP))] %>% unique
  temp.bef[ort %in% c(1,3), value := value +0.5] # Auch wenn bei  42 grad 42.5 rauskommt, ist das egal,   - so haben wir 42.5 in 107635 --> egal, weil nicht exportiert, nur fuer grenzhoehen
  temp.bef$quelle = "temp.bef"

  temp.min.b24  <- FRM_B24[,.(patstuid=PATSTUID, event = EVENT, ort = KTEMPO,value=as.numeric(KTEMPMIN))] %>% unique
  temp.min.b24[ort %in% c(1,3), value := value +0.5]
  temp.min.b24$quelle = "temp.min.b24"

  temp.max.b24  <- FRM_B24[,.(patstuid=PATSTUID, event = EVENT, ort = KTEMPO,value=as.numeric(KTEMPMAX))] %>% unique
  temp.max.b24[ort %in% c(1,3), value := value +0.5]
  temp.max.b24$quelle = "temp.max.b24"

  temp.all = rbind(temp.bef, temp.min.b24, temp.max.b24)

  temp.all[, minvalue_ort.bek := min(value[ort %in% c(1,2,3,4,5)], na.rm = T),
           .(patstuid,event)]
  # hier nehme ich wirklich das maximum vom bekannten ort, wenn der ort bei
  # temp min und temp bef unbekannt ist, nicht  lieber das vom unbekannten
  # ort --> ja, weil unbekannt oft Unzuverlaessig ist
  # temp.all[, checkme := (quelle %in% c('temp.bef', 'temp.min.b24') & all(is.na(value[ort %in% c(1,2,3,4,5)]))) &
  #            (quelle %in% c('temp.max.b24') & any(is.na(value[ort %in% c(1,2,3,4,5)])==F)) , .(patstuid,event)]
  #
  # temp.all[checkme==T] # scheint aber nicht vorzukommen

  temp.all[is.infinite(minvalue_ort.bek), minvalue_ort.bek := NA]
  temp.all[, ort.unbek := ifelse(is.na(minvalue_ort.bek), T,F)]

  # 2024-10-29 MRos: minvalue_ort.unbek is NA if minvalue_ort.bek is not NA.
  # temp.all[, minvalue_ort.unbek :=
  #            ifelse(is.na(minvalue_ort.bek),
  #                   min(value[ort %in% c(-1, 98, 99)], na.rm = T),
  #                   NA_real_),
  #          .(patstuid,event)]
  # Below, minvalue_ort.unbek is the minimum of the temp in unknown locations
  temp.all[, minvalue_ort.unbek :=
             min(value[ort %in% c(-1, 98, 99)], na.rm = T),
           .(patstuid,event)]
  temp.all[is.infinite(minvalue_ort.unbek), minvalue_ort.unbek := NA]

  # temp.all[is.na(minvalue_ort.unbek)==F, ort.unbek := T]
  temp.all[is.na(minvalue_ort.bek) & is.na(minvalue_ort.unbek), ort.unbek := F]
  temp.all[, minvalue := ifelse(is.na(minvalue_ort.bek)==F,
                                minvalue_ort.bek, minvalue_ort.unbek)]

  # 2020-02-22 MRos: this can probably result in an error or is unnecessary.
  # I have commented it out. Checked that it does not change the result.
  # temp.all[, minvalue := unique(na.omit(minvalue)),.(patstuid, event)]
  # temp.all[is.infinite(minvalue), minvalue := NA]

  temp.all[, maxvalue_ort.bek := max(value[ort %in% c(1,2,3,4,5)], na.rm = T),
           .(patstuid,event)]
  temp.all[is.infinite(maxvalue_ort.bek), maxvalue_ort.bek := NA]

  # 2024-10-29 MRos: maxvalue_ort.unnbek is NA if maxvalue_ort.bek is not NA.
  # temp.all[, maxvalue_ort.unbek :=
  #            ifelse(is.na(maxvalue_ort.bek),
  #                   max(value[ort %in% c(-1, 98, 99)], na.rm = T),
  #                   NA_real_),
  #          .(patstuid,event)]
  # Below, maxvalue_ort.unbek is the maximum of the temp in unknown locations
  temp.all[, maxvalue_ort.unbek :=
             max(value[ort %in% c(-1, 98, 99)], na.rm = T),
           .(patstuid,event)]
  temp.all[is.infinite(maxvalue_ort.unbek), maxvalue_ort.unbek := NA]

  # 2024-10-29 MRos:
  # Too high temp should be used for the computing the maximum no matter if its
  # known where the measurement was taken. This is different from a too low
  # temp. The reason is that high measured temp is less likely to be a result
  # of a measurement error.
  # temp.all[, maxvalue :=
  #            ifelse(is.na(maxvalue_ort.bek)==F,
  #                   maxvalue_ort.bek,
  #                   maxvalue_ort.unbek)
  #          ]
  temp.all[, maxvalue := pmax(maxvalue_ort.bek, maxvalue_ort.unbek,
                              na.rm = TRUE)]
  # 2020-02-22 MRos: this can probably result in an error or is unnecessary.
  # I have commented it out. Checked that it does not change the result.
  # temp.all[, maxvalue := unique(na.omit(maxvalue)),.(patstuid, event)]
  # temp.all[is.infinite(maxvalue), maxvalue := NA]

  temp.all2 = temp.all[,.(patstuid, event, ort.unbek, minvalue, maxvalue)] %>% unique
  temp.all2[,ort.unbek := as.numeric(ort.unbek)]
  # stopifnot(nrow(temp.all2[allDuplicatedEntries(paste(patstuid, event))])==0)
  stopifnot(anyDuplicated(temp.all2, by = c("patstuid", "event")) == 0)

  found_events_temp = unique(temp.all$event  )
  # Hmisc::describe(temp.all2)

  toadd_temp = dcast.data.table(temp.all2, patstuid ~ event,
                                value.var = list("minvalue","maxvalue", 'ort.unbek'))
  # showNA(toadd_temp)

  ortvars = grep("unbek", names(toadd_temp), value = T)
  for(i in ortvars) {
    toadd_temp[is.na(get(i)),(i) := 0]
  }

  setnames(toadd_temp,  paste0("ort.unbek_",found_events_temp), paste0("temp_",event2zeitpunkt(found_events_temp, returnformat = "zp_fabianref"), ".ort.unbek"))
  setnames(toadd_temp,  paste0("minvalue_",found_events_temp), paste0("temp.min_",event2zeitpunkt(found_events_temp, returnformat = "zp_fabianref")))
  setnames(toadd_temp,  paste0("maxvalue_",found_events_temp), paste0("temp.max_",event2zeitpunkt(found_events_temp, returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_temp.min)
  # stopifnot(nrow(toadd_temp[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_temp[, patstuid]) == 0)
  toadd_temp
}
