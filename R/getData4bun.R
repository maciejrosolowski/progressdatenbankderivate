#' Get information on the maximum of SHARN (urea).
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE
#' from the database of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on the SHARN, in the wide format. BUN is not available
#' because it is probably obsolete (BUN x 2.143 = Serum Harnstoff (urea)).
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' toadd_bun <- getData4bun(FRM_DIL_LABORWERTE)
#' toadd_bun
#' }
getData4bun =  function(FRM_DIL_LABORWERTE) {
  COL <- EVENT <- PATSTUID <- UNIT <- VALUE <- bun.max <- event <- patstuid <-
    value <- NULL
  sharn  <- FRM_DIL_LABORWERTE[COL=="SHARN",.(patstuid=PATSTUID, event = EVENT,
                                              UNIT, value=as.numeric(VALUE))] %>% unique
  stopifnot(all(unique(sharn$UNIT)=="mmol/l"))
  sharn$quelle ="sharn"

  # 2020-02-23, MRos bun_pre is empty because there is no "BUN" in
  # FRM_DIL_LABORWERTE.
  bun_pre  <- FRM_DIL_LABORWERTE[COL=="BUN",.(patstuid=PATSTUID, event = EVENT,
                                              UNIT, value=as.numeric(VALUE))] %>% unique
  #  BUNwurde laut peter wohlobsolet gemacht  --> ok, das ist so
  bun_pre$quelle ="bun"


  bun = rbind(sharn, bun_pre)


  bun[, bun.max := max(value, na.rm = T), .(patstuid,event)]
  bun[is.infinite(bun.max), bun.max := NA]

  bun2 = bun[,.(patstuid, event, bun.max)] %>% unique
  # stopifnot(nrow(bun2[allDuplicatedEntries(paste(patstuid, event))])==0)
  stopifnot(anyDuplicated(bun2, by = c("patstuid", "event")) == 0)

  found_events_bun = unique(bun$event)
  # Hmisc::describe(bun2)

  toadd_bun = dcast.data.table(bun2, patstuid ~ event, value.var = 'bun.max')

  setnames(toadd_bun,  as.character(found_events_bun),
           paste0("bun_",
                  event2zeitpunkt(found_events_bun, returnformat = "zp_fabianref")))

  # Hmisc::describe(toadd_bun)
  # stopifnot(nrow(toadd_bun[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_bun, by = c("patstuid")) == 0)
  toadd_bun
}
