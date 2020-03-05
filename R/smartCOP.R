#' Compute the smartCOP score.
#'
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_VIS data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_DIL_LABORWERTE data.table containing the table with the same
#'  name from the database of the PROGRESS study
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_PROBAND data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_CLIN data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_OXYGENIND_SINGLE data.table containing the table with the same
#'  name from the database of the PROGRESS study
#' @param FRM_O2A data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref. Currently, only
#' zp_fabian = "auf_in_d-1_in_d0" is possible.
#'
#' @return a named list with components: input and out. input is a data.table
#' in the wide format (one row per patient), containing the data used for
#' computing the smartCOP score. out is a data.table with one row
#' corresponding to one patient, identified by the
#' PATSTUID. The column smart.COP contains the value of smartCOP score.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' FRM_VIS <- readxl::read_excel(excel_fn, 'FRM_VIS')
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
#' DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
#' DID_OXYGENIND_SINGLE <- readxl::read_excel(excel_fn, 'DID_OXYGENIND_SINGLE')
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_VIS)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(FRM_B24)
#' data.table::setDT(DID_PROBAND)
#' data.table::setDT(DID_CLIN)
#' data.table::setDT(DID_OXYGENIND_SINGLE)
#' data.table::setDT(FRM_O2A)
#' erg <- smartCOP(FRM_RR, FRM_BEF, FRM_VIS, FRM_DIL_LABORWERTE, FRM_B24,
#' DID_PROBAND, DID_CLIN, DID_OXYGENIND_SINGLE, FRM_O2A,
#' zp_fabian = "auf_in_d-1_in_d0")
#' erg
#' }
smartCOP <- function (FRM_RR,FRM_BEF, FRM_VIS,FRM_DIL_LABORWERTE,FRM_B24,
                      DID_PROBAND,DID_CLIN,DID_OXYGENIND_SINGLE,FRM_O2A,
                      zp_fabian="auf_in_d-1_in_d0") {
  # due to non-standard evaluation notes in R CMD check
  patstuid <- NULL
  # SMART-COP, urspruenglich von Katrin  #eingebaut am 15. Maerz 2017
  if ( !(zp_fabian %in% c("auf_in_d-1_in_d0")) ) {
    stop("ERROR: variable zp_fabian must be set to auf_in_d-1_in_d0
         It's not possible to calculate it for another time point!")
  } # Nach absprache mit peter 4.6.19 auffuellen nicht konsequent
  # auf_in_d-1_in_d0, sondern nur dort, wo es katrin gemacht hat,
  # um vergleichbarkeit basispaper zu optimieren


  toadd_sysbp.min = getData4sysbp.min (FRM_RR)
  toadd_multl_inf = getData4multl_inf (FRM_BEF, FRM_VIS)
  toadd_alb = getData4albumin(FRM_DIL_LABORWERTE)
  toadd_afrq.min =getData4afrqMin(FRM_B24,FRM_BEF)
  toadd_afrq.max =  getData4afrqMax(FRM_B24,FRM_BEF)
  toadd_agesex = getData4age.sex (DID_PROBAND)
  toadd_hfrq.min =  getData4hfrqMin(FRM_B24,FRM_BEF)
  toadd_hfrq.max =  getData4hfrqMax(FRM_B24,FRM_BEF)
  toadd_verwirrt =   getData4verwirrt(FRM_BEF)
  toadd_gcs =  getData4gcs (DID_CLIN)
  toadd_oxyIndex.min = getData4oxyIndex(DID_OXYGENIND_SINGLE)
  toadd_art.ph= getData4art.ph(FRM_B24, FRM_O2A)

  DAT = merge(toadd_sysbp.min, toadd_multl_inf, by= "patstuid", all = T,
              sort = F)
  DAT = merge(DAT, toadd_alb, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_agesex, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_hfrq.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_hfrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_gcs, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_oxyIndex.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_art.ph, by= "patstuid", all = T,sort = F)

  # stopifnot(nrow(DAT[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(DAT[, patstuid]) == 0)
  setDF(DAT)

  #systolischer Blutdruck < 90 mmHG -> 2 Punkte -> ok
  sbp<-DAT$sysbp.min_d0
  myFilt<-is.na(sbp)
  sbp[myFilt]<-DAT$sysbp.min_auf[myFilt]
  SBP.p<-as.numeric(sbp<90) * 2

  #Multilobaeres Infiltrat: vorhanden -> 1 Punkt -> ok
  mi<-DAT$multl_inf_d0
  myFilt<-is.na(mi)
  mi[myFilt]<-DAT$multl_inf_auf[myFilt]
  MI.p<-as.numeric(mi)

  #Albumin < 3.5 g/dl -> 1 Punkt -> ok (nur 405 Werte zum Zeitpunkt d0, keine
  # zur Aufnahme)

  alb<-DAT$alb_d0 #KAtrin: in g/l laut Peter
  Alb.p<-as.numeric((alb/10) < 3.5)

  #Atemfrequenz altersabhaengig -> 1 Punkt -> ok
  af<-pmax(DAT$afrq.min_d0,DAT$afrq.max_d0,na.rm=T)
  myFilt<-is.na(af)
  dummy<-pmax(DAT$afrq.min_auf,DAT$afrq.max_auf,na.rm=T)
  af[myFilt]<-dummy[myFilt]
  # AF.p<-vector(mode="numeric",length=nrow(DAT))
  # myFilt<-DAT$age<=50
  # AF.p[myFilt]<-as.numeric(af[myFilt]>=25)
  # AF.p[!myFilt]<-as.numeric(af[!myFilt]>=30)
  AF.p = ifelse(DAT$age<=50, as.numeric(af>=25), as.numeric(af>=30))

  #Herzfrequenz >= 125 Schlaege -> 1 Punkt -> ok
  hrf<-pmax(DAT$hfrq.min_d0,DAT$hfrq.max_d0,na.rm=T)
  myFilt<-is.na(hrf)
  dummy<-pmax(DAT$hfrq.min_auf,DAT$hfrq.max_auf,na.rm=T)
  hrf[myFilt]<-dummy[myFilt]
  HRF.p<-as.numeric(hrf>=125)

  #Mental Status verwirrt oder Glasgow Coma Scale < 15 -> 1 Punkt -> ok
  verwirrt<-DAT$verwirrt_d0
  myFilt<-is.na(verwirrt)
  verwirrt[myFilt]<-DAT$verwirrt_auf[myFilt]
  verwirrt[is.na(verwirrt)]<-0
  MS.p<-as.numeric(verwirrt| (DAT$gcs_d0<15))

  #Sauerstoffstaettigung PaO2/FiO2 < 250mmHg /333mmHG -> 2 Punkte -> ok

  oxiIndex<-DAT$oxyIndex.min_d0
  # O2.p<-vector(mode="numeric",length=nrow(DAT))
  # myFilt<-DAT$age<=50
  # O2.p[myFilt]<-as.numeric(oxiIndex[myFilt]<333)*2
  # O2.p[!myFilt]<-as.numeric(oxiIndex[!myFilt]<250)*2
  #
  O2.p = ifelse(DAT$age<=50, as.numeric(oxiIndex<333)*2,
                as.numeric(oxiIndex<250)*2)


  #arterielle pH < 7.35 -> 2 Punkte -> ok
  aph<-DAT$art.ph.min_d0
  myFilt<-is.na(aph)
  aph[myFilt]<-DAT$art.ph.min_auf[myFilt]
  apH.p<-as.numeric(aph<7.35) * 2

  #Gesamtscore berechnen
  dummy<-cbind(SBP.p,MI.p,Alb.p,AF.p,HRF.p,MS.p,O2.p,apH.p)
  smart.COP<-apply(dummy,1,function(x) sum(x,na.rm=T))

  #table(smart.COP,useNA="ifany")
  #hist(smart.COP)

  # return(smart.COP)
  res = data.table(smart.COP)
  res$PATSTUID  = DAT$patstuid
  res$event = zeitpunkt2event(zp_fabian)
  # 2020-03-05 MRos: replace call to moveColFront for no dependency on toolboxH
  # res = moveColFront(res,c( "PATSTUID", 'event'))
  res <- data.table::setcolorder(res, neworder = c( "PATSTUID", 'event'))
  erg = c()
  erg$input  = DAT
  erg$input2 = c()

  erg$out  = res
  erg

  #completeness of score
  # dummy<-cbind(!is.na(SBP.p),!is.na(MI.p),!is.na(Alb.p),!is.na(AF.p),
  # !is.na(HRF.p),!is.na(MS.p),!is.na(O2.p),!is.na(apH.p))
  # com<-apply(dummy,1,function(x) sum(x)/8)
  # sum(com>=0.5)
  # sum(com>=0.5)/1532
  # sum(com>=0.75)
  # sum(com>=0.75)/1532
  # sum(com>=0.9)
  # sum(com>=0.9)/1532
  # sum(com>=1)
  # sum(com>=1)/1532
}
