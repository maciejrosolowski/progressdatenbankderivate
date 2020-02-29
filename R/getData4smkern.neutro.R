#' get the count of segmented neutrophils
#'
#'The conunt is per 1 microliter.
#'
#' @param FRM_DIL_LABORWERTE data.table containing the table FRM_DIL_LABORWERTE
#' from the database of the PROGRESS study
#'
#' @return data.table in the wide format. Each row corresponds to one patient
#' and the columns contain the count of segmented neutrophils at a given time.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' toadd_smkern.neutro <- getData4smkern.neutro(FRM_DIL_LABORWERTE)
#' toadd_smkern.neutro
#' }
getData4smkern.neutro <- function(FRM_DIL_LABORWERTE) {
  # due to non-standard evaluation notes in R CMD check
  COL <- EVENT <- PATSTUID <- DIFFBB <- UNIT <- VALUE <- NULL
  # 44. segmentkernige neutrophile Granulozyten
  smkern.neutro  <- FRM_DIL_LABORWERTE[
    COL=="SNGRANU", .(patstuid=PATSTUID, event=EVENT,UNIT ,DIFFBB,
                      value= as.numeric(VALUE))] %>% unique
  # library(assertr)
  # smkern.neutro[allDuplicatedEntries(paste(patstuid, event))] %>% verify(nrow(.)==0)
  stopifnot(anyDuplicated(smkern.neutro, by = c("patstuid", "event")) == 0)

  found_events_smkern.neutro = unique(smkern.neutro$event  )
  # Hmisc::describe(smkern.neutro)

  toadd_smkern.neutro = dcast.data.table(smkern.neutro, patstuid ~ event)
  setnames(toadd_smkern.neutro,  as.character(found_events_smkern.neutro),
           paste0("smkern.neutro_",event2zeitpunkt(found_events_smkern.neutro,
                                                   returnformat = "zp_fabianref")))
  # Hmisc::describe(toadd_smkern.neutro)
  # stopifnot(nrow(toadd_smkern.neutro[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_smkern.neutro, by = c("patstuid")) == 0)
  toadd_smkern.neutro[]
}
