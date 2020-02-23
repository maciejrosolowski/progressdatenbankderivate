#' Get data on pleural effusion.
#'
#' @param FRM_BEF data.table containing the table FRM_BEF from the database
#' of the PROGRESS study
#' @param FRM_VIS data.table containing the table FRM_VIS from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on pleural effusion (PLEUERGUSS from FRM_BEF, FRM_VIS),
#' in the wide format.
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
#' toadd_pleu <- getData4pleu(FRM_BEF, FRM_VIS)
#' toadd_pleu
#' }
getData4pleu = function(FRM_BEF, FRM_VIS){
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- PLEUERGUSS <- XTHORPLEU <- event <- value <- NULL
  pleu1  <- FRM_BEF[,.(patstuid=PATSTUID, event = EVENT,
                       value=as.numeric(PLEUERGUSS))] %>% unique
  pleu1[,.N, event]
  pleu1$quelle ="pleu1"

  pleu2  <- FRM_VIS[,.(patstuid=PATSTUID, event = EVENT,
                       value=as.numeric(XTHORPLEU))] %>% unique
  pleu2[,.N, event]
  pleu2$quelle ="pleu2"

  pleu = rbind(pleu1, pleu2)
  pleu[value %in% c(-1,99), value := NA]
  # stopifnot(nrow(pleu[allDuplicatedEntries(paste(patstuid, event))])==0)
  stopifnot(anyDuplicated(pleu, by = c("patstuid", "event")) == 0)


  found_events_pleu = unique(pleu$event)
  # Hmisc::describe(pleu)

  toadd_pleu = dcast.data.table(pleu, patstuid ~ event, value.var = 'value')

  setnames(toadd_pleu,  as.character(found_events_pleu),
           paste0("pleu_erg_",event2zeitpunkt(found_events_pleu,
                                              returnformat = "zp_fabianref")))

  # Hmisc::describe(toadd_pleu)
  # stopifnot(nrow(toadd_pleu[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_pleu, by = c("patstuid")) == 0)
  toadd_pleu
}
