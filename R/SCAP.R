#' Compute the SCAP score.
#'
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_O2A data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_DIL_LABORWERTE data.table containing the table with the same
#'  name from the database of the PROGRESS study
#' @param DID_CLIN data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_PROBAND data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_VIS data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_OXYGENIND_SINGLE data.table containing the table with the same
#'  name from the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref. Currently, only
#' zp_fabian = "auf_in_d-1_in_d0" is possible.
#'
#' @return a named list with components: input and out. input is a data.table
#' in the wide format (one row per patient), containing the data used for
#' computing the SCAP score. out is a data.table with one row
#' corresponding to one patient, identified by the
#' PATSTUID. The column SCAP contains the value of SCAP score. The score is
#' available, if more than 50% of its subscores, i.e., 5 or more are available.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
#' DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
#' DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
#' FRM_VIS <- readxl::read_excel(excel_fn, 'FRM_VIS')
#' DID_OXYGENIND_SINGLE <- readxl::read_excel(excel_fn, 'DID_OXYGENIND_SINGLE')
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_O2A)
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(DID_CLIN)
#' data.table::setDT(DID_PROBAND)
#' data.table::setDT(FRM_VIS)
#' data.table::setDT(DID_OXYGENIND_SINGLE)
#' erg <- SCAP(FRM_B24, FRM_O2A, FRM_RR, FRM_BEF, FRM_DIL_LABORWERTE,
#' DID_CLIN, DID_PROBAND, FRM_VIS, DID_OXYGENIND_SINGLE,
#' zp_fabian = "auf_in_d-1_in_d0")
#' erg
#' }
SCAP <- function(FRM_B24, FRM_O2A,FRM_RR, FRM_BEF,FRM_DIL_LABORWERTE,
                  DID_CLIN,DID_PROBAND,FRM_VIS, DID_OXYGENIND_SINGLE,
                 zp_fabian="auf_in_d-1_in_d0") {
  # due to non-standard evaluation notes in R CMD check
  patstuid <- SCAP <- NULL
  #SCAP, urspruenglich von Katrin  eingebaut am 15. Maerz 2017
  if ( !(zp_fabian %in% c("auf_in_d-1_in_d0")) ) {
    stop("ERROR: variable zp_fabian must be set to auf_in_d-1_in_d0 It's not
         possible to calculate it for another time point!")
  } # Nach absprache mit peter 4.6.19 auffuellen nicht konsequent
  # auf_in_d-1_in_d0, sondern nur dort, wo es katrin gemacht hat,
  # um vergleichbarkeit basispaper zu optimieren

  toadd_art.ph= getData4art.ph(FRM_B24, FRM_O2A)
  toadd_sysbp.min = getData4sysbp.min (FRM_RR)
  toadd_afrq.min =getData4afrqMin(FRM_B24,FRM_BEF)
  toadd_afrq.max =  getData4afrqMax(FRM_B24,FRM_BEF)
  toadd_bun = getData4bun(FRM_DIL_LABORWERTE )
  toadd_verwirrt =   getData4verwirrt(FRM_BEF)
  toadd_gcs =  getData4gcs (DID_CLIN)
  toadd_agesex = getData4age.sex (DID_PROBAND)
  toadd_multl_inf = getData4multl_inf (FRM_BEF, FRM_VIS)
  toadd_oxyIndex.min = getData4oxyIndex(DID_OXYGENIND_SINGLE)

  DAT = merge(toadd_art.ph, toadd_sysbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_bun, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_gcs, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_agesex, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_multl_inf, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_oxyIndex.min, by= "patstuid", all = T,sort = F)

  # stopifnot(nrow(DAT[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(DAT[, patstuid]) == 0)
  setDF(DAT)

  #arterielle pH < 7.30 -> 13 Punkte -> ok
  aph<-DAT$art.ph.min_d0
  myFilt<-is.na(aph)
  aph[myFilt]<-DAT$art.ph.min_auf[myFilt]
  apH.p<-as.numeric(aph<7.30) * 13

  #systolischer Blutdruck < 90 mmHG -> 11 Punkte -> ok
  sbp<-DAT$sysbp.min_d0
  myFilt<-is.na(sbp)
  sbp[myFilt]<-DAT$sysbp.min_auf[myFilt]
  SBP.p<-as.numeric(sbp<90) * 11

  #Atemfrequenz > 30/min -> 9 Punkte -> ok
  af<-pmax(DAT$afrq.min_d0,DAT$afrq.max_d0,na.rm=T)
  myFilt<-is.na(af)
  dummy<-pmax(DAT$afrq.min_auf,DAT$afrq.max_auf,na.rm=T)
  af[myFilt]<-dummy[myFilt]
  AF.p<-as.numeric(af>30) * 9

  #Blutharnstoff-Stickstoff > 30 mg/dl -> 5 Punkte -> Peter will nicht
  # Berechnung aus Harnstoff, BUN gibt es direkt vom Labor
  bhss<-DAT$bun_d0
  myFilt<-is.na(bhss)
  bhss[myFilt]<-DAT$bun_auf[myFilt]
  BHSS.p<-as.numeric((bhss/0.357)>30) * 5

  #Mental Status verwirrt oder Glasgow Coma Scale < 15 -> 5 Punkte -> ok
  verwirrt<-DAT$verwirrt_d0
  myFilt<-is.na(verwirrt)
  verwirrt[myFilt]<-DAT$verwirrt_auf[myFilt]
  verwirrt[is.na(verwirrt)]<-0
  MS.p<-as.numeric(verwirrt| (DAT$gcs_d0<15)) * 5


  #Sauerstoffsaettigung PaO2/FiO2 < 250mmHg -> 6 Punkte -> ok
  #min von WERT egal welche COL, aber Event 1 oder 3, multiplizieren mit
  # 1/0.1333224 -> 7.500615

  oxiIndex<-DAT$oxyIndex.min_d0 # HK 5.6.19 entgegen der Vorschrift wurde nicht
  # das minimum von EVENT1 und 3 gebildet. Nach absprache mit peter 4.6.19
  # auffuellen nicht konsequent auf_in_d-1_in_d0, sondern nur dort, wo es
  # katrin gemacht hat, um vergleichbarkeit basispaper zu optimieren
  O2.p<-as.numeric(oxiIndex<250) * 6

  #Alter >= 80 Jahre -> 5 Punkte -> ok
  Age.p<-as.numeric(DAT$age>=80.0) * 5

  #Multilobaeres Infiltrat: vorhanden -> 5 Punkte -> ok
  mi<-DAT$multl_inf_d0
  myFilt<-is.na(mi)
  mi[myFilt]<-DAT$multl_inf_auf[myFilt]
  MI.p<-as.numeric(mi)*5

  #Gesamtscore berechnen
  # dummy<-cbind(apH.p,SBP.p,AF.p,BHSS.p,MS.p,O2.p,Age.p,MI.p)
  # SCAP<-apply(dummy,1,function(x) sum(x,na.rm=T))

  # res = data.table(SCAP, dummy)
  # res$PATSTUID  = DAT$patstuid
  # res$EVENT = zeitpunkt2event(zp_fabian)

  res <- data.table(PATSTUID = DAT$patstuid,
                    EVENT = zeitpunkt2event(zp_fabian),
                    apH.p, SBP.p, AF.p, BHSS.p, MS.p, O2.p, Age.p, MI.p)
  # 50% rule. > 50% of the subscores have to be non-NA for the score
  # to be non-NA. 2020-07-01.
  res[, SCAP := ifelse(rowSums(!is.na(.SD)) >= 5,
                       rowSums(.SD, na.rm = TRUE), NA_integer_),
      .SDcols = c("apH.p", "SBP.p", "AF.p", "BHSS.p", "MS.p", "O2.p",
                  "Age.p", "MI.p")]

  # 2020-03-05 MRos: replace call to moveColFront for no dependency on toolboxH
  # res = moveColFront(res,c( "PATSTUID", 'event'))
  res <- data.table::setcolorder(res, neworder = c( "PATSTUID", "EVENT"))
  erg = c()
  erg$input  = DAT
  erg$input2 = c()

  erg$out  = res
  erg


  #completeness of score
  # dummy<-cbind(!is.na(apH.p),!is.na(SBP.p),!is.na(AF.p),!is.na(BHSS.p),
  # !is.na(MS.p),!is.na(O2.p),!is.na(Age.p),!is.na(MI.p))
  # com<-apply(dummy,1,function(x) sum(x)/8)
  # sum(com>=0.5)
  # sum(com>=0.5)/1532
  # sum(com>=0.75)
  # sum(com>=0.75)/1532
  # sum(com>=0.9)
  # sum(com>=0.9)/1532
  # sum(com>=1)
  # sum(com>=1)/1532

  # 2020-03-05 MRos: Checking the results against the results obtained earlier
  # by Katrin (corrected by Holger)
  # load("/net/ifs1/san_projekte/projekte/genstat/02_projekte/1208_progress/
  # Basispaper/Analysen/03_score_calculation_Katrin_CHECK_hk2.RData")
  # SCAP_scores <- copy(erg$out)
  # library(data.table)
  # SCAP_scores[, PATSTUID := as.character(PATSTUID)]
  # setDT(SCORES, keep.rownames = "PATSTUID")
  # comp_SCAP <- merge(SCAP_scores[, .(PATSTUID, SCAP)],
  # SCORES[, .(PATSTUID, SCAP.auf_in_d0)], by = "PATSTUID",
  # all = TRUE, sort = FALSE)
  # table(comp_SCAP[, SCAP == SCAP.auf_in_d0], useNA = "always")
  # FALSE  TRUE  <NA>
  #    20  1512   681
  # comp_SCAP[SCAP != SCAP.auf_in_d0,]
}
