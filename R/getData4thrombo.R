#' get thrombocyte count from the database of the PROGRESS study
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE from the database
#' of the PROGRESS study
#' @param DID_CLIN as above
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' DID_CLIN <- readxl::read_excel(excel_fn, "DID_CLIN")
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(DID_CLIN)
#' getData4thrombo(FRM_DIL_LABORWERTE, DID_CLIN)
#' }
#'
getData4thrombo <- function(FRM_DIL_LABORWERTE, DID_CLIN) {
  # 53. Thrombozyten
  thrombo <- FRM_DIL_LABORWERTE[COL=="THROMBO", .(
    patstuid=PATSTUID, event=EVENT,UNIT ,
    DIFFBB, value= as.numeric(VALUE),
    quelle = "thrombo")] %>% unique
  thrombo.lab <- FRM_DIL_LABORWERTE[COL=="THROMBOMIN", .(
    patstuid=PATSTUID, event=EVENT,UNIT,
    DIFFBB, value= as.numeric(VALUE), quelle = "thrombo.lab")] %>% unique
  thrombo.cli <- DID_CLIN[CLIN_PARAM=="THROMBO-MIN", .(
    patstuid=PATSTUID, event=EVENT,PARAM_CODE,
    value= as.numeric(WERT), quelle = "thrombo.cli")] %>% unique

  # library(assertr)
  # thrombo[allDuplicatedEntries(paste(patstuid, event))] %>% verify(nrow(.)==0)
  # thrombo.lab[allDuplicatedEntries(paste(patstuid, event))]%>% verify(nrow(.)==0)
  # thrombo.cli[allDuplicatedEntries(paste(patstuid, event))]%>% verify(nrow(.)==0)
  # alternative code, not using the packages assertr and toolboxH
  stopifnot(anyDuplicated(thrombo, by = c("patstuid", "event")) == 0)
  stopifnot(anyDuplicated(thrombo.lab, by = c("patstuid", "event")) == 0)
  stopifnot(anyDuplicated(thrombo.cli, by = c("patstuid", "event")) == 0)

  thrombo.all = rbind(thrombo[,-c("UNIT", "DIFFBB")], thrombo.lab[,-c("UNIT", "DIFFBB")], thrombo.cli[,-c("PARAM_CODE")])
  # Hmisc::describe(thrombo.all)

  found_events_thrombo = unique(thrombo$event  )

  toadd_thrombo = dcast.data.table(thrombo, patstuid ~ event)
  setnames(toadd_thrombo,  as.character(found_events_thrombo),
           paste0("thrombo_dbb_",event2zeitpunkt(found_events_thrombo, returnformat = "zp_fabianref")))
  # stopifnot(nrow(toadd_thrombo[allDuplicatedEntries(patstuid)])==0)
  # alternative code, not using the package toolboxH
  stopifnot(anyDuplicated(toadd_thrombo[, patstuid]) == 0)
  toadd_thrombo

  thrombo.all.min = thrombo.all[, .(minvalue = min(value, na.rm = T)), .(patstuid,event)]
  thrombo.all.min[is.infinite(minvalue), minvalue := NA]
  found_events_thrombo.lab = unique(thrombo.all.min$event  )
  # Hmisc::describe(thrombo.all.min)
  toadd_thrombo.all.min = dcast.data.table(thrombo.all.min, patstuid ~ event, value.var = "minvalue")
  setnames(toadd_thrombo.all.min,  as.character(found_events_thrombo.lab),
           paste0("thrombo_min_",event2zeitpunkt(found_events_thrombo.lab, returnformat = "zp_fabianref")))
  # stopifnot(nrow(toadd_thrombo.all.min[allDuplicatedEntries(patstuid)])==0)
  # alternative code, not using toolboxH
  stopifnot(anyDuplicated(toadd_thrombo.all.min[, patstuid]) == 0)
  toadd_thrombo.all.min

  dat = merge(toadd_thrombo, toadd_thrombo.all.min, by = 'patstuid', all = T,sort = F)
  # stopifnot(nrow(dat[allDuplicatedEntries(patstuid)])==0)
  # alternative code, not using toolboxH
  stopifnot(anyDuplicated(dat[, patstuid]) == 0)
  dat
}

utils::globalVariables(c("event", "UNIT", "COL", "DIFFBB", "VALUE",
                         "PARAM_CODE", "minvalue"))
