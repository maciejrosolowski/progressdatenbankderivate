#' Compute the quickSOFA score.
#'
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_CLIN data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref. Currently, only zp_fabian = "auf_in_d0"
#' is possible.
#'
#' @return a named list with components: input and out. input is a data.table
#' in the wide format (one row per patient), containing the data used for
#' computing the quickSOFA score. out is a data.table with one row
#' corresponding to one patient, identified by the
#' PATSTUID. The column res contains the value of quickSOFA.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(DID_CLIN)
#' erg_d0 <- quickSOFA(FRM_RR, FRM_B24, FRM_BEF,
#' DID_CLIN, zp_fabian = "auf_in_d0")
#' erg_d0
#' }
quickSOFA <- function (FRM_RR,FRM_B24,FRM_BEF,DID_CLIN, zp_fabian="auf_in_d0") {
  # due to non-standard evaluation notes in R CMD check
  CLIN_PARAM <- EVENT <- PATSTUID <- WERT <- patstuid <- qSOFA <- NULL
  #  qSOFA (quick SOFA), urspruenglich von Katrin eingebaut am 09. Dezember 2016

  if ( !(zp_fabian %in% c("auf_in_d0")) ) {
    stop("ERROR: variable zp_fabian must be set to d0. It's not possible to
         calculate it for another zp_fabian point!")
  } # Nach absprache mit peter 4.6.19 auffuellen nicht konsequent
  # auf_in_d-1_in_d0, sondern nur dort, wo es katrin gemacht hat, um
  # vergleichbarkeit basispaper zu optimieren

  toadd_sysbp.min = getData4sysbp.min (FRM_RR)
  toadd_afrq.max =  getData4afrqMax(FRM_B24,FRM_BEF)
  toadd_afrq.min =getData4afrqMin(FRM_B24,FRM_BEF)
  toadd_verwirrt =   getData4verwirrt(FRM_BEF)
  toadd_gcs =  getData4gcs (DID_CLIN)

  #Zusammenbaun
  DAT = merge(toadd_sysbp.min, toadd_afrq.max, by= "patstuid", all = T,
              sort = F)
  DAT = merge(DAT, toadd_afrq.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_gcs, by= "patstuid", all = T,sort = F)
  DAT

  # stopifnot(nrow(DAT[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(DAT[, patstuid]) == 0)
  setDF(DAT)
  #ab hier katrin
  bp1<-DAT$sysbp.min_d0
  myFilt<-is.na(bp1)
  bp1[myFilt]<-DAT$sysbp.min_auf[myFilt]
  bloodpressurePoint<-as.numeric(bp1 <= 100)

  hrp<-pmax(DAT$afrq.min_d0,DAT$afrq.max_d0,na.rm=T)
  myFilt<-is.na(hrp)
  dummy<-pmax(DAT$afrq.min_auf,DAT$afrq.max_auf,na.rm=T)
  hrp[myFilt]<-dummy[myFilt]
  highRespRatePoint<-as.numeric(hrp >= 22)

  # Glasgow-Coma-Scale < 15, Daten nicht fuer Aufnahme vorhanden
  # keine NAs in gcs_d0 vorhanden
  # falls ein Patient bei Aufnahme oder d0 verwirrt war, bekommt er auch den
  # Punkt fuer glasgow coma scale
  verwirrt<-DAT$verwirrt_d0
  myFilt<-is.na(verwirrt)
  verwirrt[myFilt]<-DAT$verwirrt_auf[myFilt]
  verwirrt[is.na(verwirrt)]<-0
  gcsPoint<-as.numeric(DAT$gcs_d0<15 | verwirrt )
  #hier noch auf verwirrt_auf und verwirrt_d0 pruefen, falls verwirrt,
  # dann Punkt

  # dummy<-cbind(bloodpressurePoint,highRespRatePoint,gcsPoint)
  # res<-apply(dummy,1,function(x) sum(x,na.rm=T))

  #completeness of score
  # verwirrt<-DAT$verwirrt_d0
  # myFilt<-is.na(verwirrt)
  # verwirrt[myFilt]<-DAT$verwirrt_auf[myFilt]
  # gcsPoint<-as.numeric(DAT$gcs_d0<15 | verwirrt )
  # dummy<-cbind(!is.na(bloodpressurePoint),!is.na(highRespRatePoint),
  # !is.na(gcsPoint))
  # com<-apply(dummy,1,function(x) sum(x)/3)
  # sum(com>=0.5)
  # sum(com>=0.5)/1532
  # sum(com>=0.75)
  # sum(com>=0.75)/1532
  # sum(com>=0.9)
  # sum(com>=0.9)/1532
  # sum(com>=1)
  # sum(com>=1)/1532

  # res = data.table(qSOFA = res, dummy)
  # res$PATSTUID  = DAT$patstuid
  # res$EVENT = zeitpunkt2event(zp_fabian)

  res <- data.table(PATSTUID = DAT$patstuid,
                    EVENT = zeitpunkt2event(zp_fabian),
                    bloodpressurePoint, highRespRatePoint, gcsPoint)
  # 50% rule. > 50% of the subscores have to be non-NA for the score
  # to be non-NA
  res[, qSOFA := ifelse(rowSums(!is.na(.SD)) >= 2,
                        rowSums(.SD, na.rm = TRUE), NA_integer_),
      .SDcols = c("bloodpressurePoint", "highRespRatePoint", "gcsPoint")]

  # 2020-03-03 MRos: replace call to moveColFront for no dependency on toolboxH
  # res = moveColFront(res,c( "PATSTUID", 'event'))
  res <- data.table::setcolorder(res, neworder = c( "PATSTUID", "EVENT"))
  erg = c()
  erg$input  = DAT
  erg$input2 = c()

  erg$out  = res
  erg
}
