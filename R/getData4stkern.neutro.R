#' get the count of banded neutrophils
#'
#'The conunt is per 1 microliter.
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE
#' from the database of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain the count of banded neutrophils at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' toadd_stkern.neutro <- getData4stkern.neutro(FRM_DIL_LABORWERTE)
#' toadd_stkern.neutro
#' }
getData4stkern.neutro <- function(FRM_DIL_LABORWERTE) {
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- DIFFBB <- UNIT <- VALUE <- NULL
  # 45. stabkernige neutrophile Granulozyten
  stkern.neutro  <- FRM_DIL_LABORWERTE[
    COL=="SNEUTRO", .(patstuid=PATSTUID, event=EVENT,UNIT ,DIFFBB,
                      value= as.numeric(VALUE))] %>% unique
  # library(assertr)
  # stkern.neutro[allDuplicatedEntries(paste(patstuid, event))] %>% verify(nrow(.)==0)
  stopifnot(anyDuplicated(stkern.neutro, by = c("patstuid", "event")) == 0)

  found_events_stkern.neutro = unique(stkern.neutro$event  )
  # Hmisc::describe(stkern.neutro)

  toadd_stkern.neutro = dcast.data.table(stkern.neutro, patstuid ~ event)
  setnames(toadd_stkern.neutro,  as.character(found_events_stkern.neutro),
           paste0("stkern.neutro_",event2zeitpunkt(found_events_stkern.neutro,
                                                   returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_stkern.neutro)
  # stopifnot(nrow(toadd_stkern.neutro[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_stkern.neutro, by = c("patstuid")) == 0)
  toadd_stkern.neutro
}
