#' Get the data on the (mechanical) ventilation.
#'
#' The columns in the returned data.table contain detailed information on
#' the method by which the ventilation was applied. The last set of columns,
#' bea.auf, bea.d0 and so on, say if ventilation was used at all,
#' i.e., by any method.
#'
#' @param FRM_BEAT data.table containing the table so named from the database
#' of the PROGRESS study
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' data on the ventilation, in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BEAT <- readxl::read_excel(excel_fn, 'FRM_BEAT', guess_max = 10e5)
#' data.table::setDT(FRM_BEAT)
#' toadd_beat <- getData4beat(FRM_BEAT)
#' toadd_beat
#' }
getData4beat <- function(FRM_BEAT) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- INTUB <- MASKE <- PATBEATM <- PATSTUID <- TRACHKAN <- atall <-
    event <- falsch <- intub <- maske <- newname <- old <- patbea <- trkan <-
    typ <- NULL
  # 65 Beatmung
  beat <- FRM_BEAT[,.(patstuid=PATSTUID, event = EVENT,
                      maske = MASKE==1,
                      intub = INTUB==1,
                      trkan = TRACHKAN==1,
                      patbea= ifelse(PATBEATM %in% c(-1,99), NA,
                                     as.logical(PATBEATM)))] %>% unique
  # 2020-03-04 MRos: trkan==1 is OK because in R (TRUE == 1) is TRUE
  beat[,atall := ifelse(patbea==T | maske==T | intub==T | trkan==1, TRUE,
                        ifelse(patbea==FALSE, FALSE, NA))]

  # showNA(beat)
  beat[is.na(patbea), maske :=NA]
  beat[is.na(patbea), intub :=NA]
  beat[is.na(patbea), trkan :=NA]
  # showNA(beat)

  # Widerspruch
  # 2020-03-04 MRos: falsch == TRUE if patbea == TRUE but maske, intub, trkan
  # are FALSE.
  beat[,falsch := maske==F& intub==F & trkan==F & atall==T]
  # beat[,.N, falsch ]
  beat[is.na(falsch), falsch := F]

  # beat[falsch==T,.N,maske]
  beat[falsch==T,maske := NA]
  beat[falsch==T,intub := NA]
  beat[falsch==T,trkan := NA]
  beat[falsch==T,atall := NA]

  # Hmisc::describe(beat)


  toadd_beat = dcast.data.table(beat, patstuid ~ event,
                                value.var =
                                  c("maske", "intub", "trkan", "atall", "patbea"))
  renaming= data.table(old = setdiff(names(toadd_beat),"patstuid"))
  # renaming[,typ:=stringr::str_split(old, "_") %>% sapply(.,"[",1)]
  renaming[, typ := strsplit(old, "_") %>% sapply(.,"[",1)]
  # renaming[,event:=stringr::str_split(old, "_") %>% sapply(.,"[",2)]
  renaming[, event := strsplit(old, "_") %>% sapply(.,"[",2)]
  # renaming[
  #   ,newname:=paste0(event2zeitpunkt(event, returnformat = "zp_fabianref"),
  #                    "_beat_",typ)]
  # put the time point at the end (not at the beginning)
  renaming[
    ,newname:=paste0(typ, "_beat_",
                     event2zeitpunkt(event, returnformat = "zp_fabianref")
                     )]
  renaming[
    typ =="atall", newname :=
      paste0("bea_", event2zeitpunkt(event, returnformat = "zp_fabianref"))]
  # 2020-05-13 MRos: zp_fabian as suffix (not prefix) for curb65.fct
  renaming[typ == "patbea", newname := paste(typ, event2zeitpunkt(
    event, returnformat = "zp_fabianref"), sep = "_")]

  setnames(toadd_beat,  renaming$old, renaming$newname)
  toadd_beat
}
