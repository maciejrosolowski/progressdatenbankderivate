#' Compute the Halm score.
#'
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_O2A data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_O2P data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEAT data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_CLIN data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref. Currently, only
#' zp_fabian = "auf_in_d-1_in_d0" is possible.
#'
#' @return a named list with components: input and out. input is a data.table
#' in the wide format (one row per patient), containing the data used for
#' computing the Halm score. out is a data.table with one row
#' corresponding to one patient, identified by the
#' PATSTUID. The column halm contains the value of Halm.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' FRM_O2P <- readxl::read_excel(excel_fn, 'FRM_O2P')
#' FRM_BEAT <- readxl::read_excel(excel_fn, 'FRM_BEAT')
#' DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_O2A)
#' data.table::setDT(FRM_O2P)
#' data.table::setDT(FRM_BEAT)
#' data.table::setDT(DID_CLIN)
#' erg <- HalmScore(FRM_B24, FRM_BEF, FRM_RR, FRM_O2A, FRM_O2P, FRM_BEAT,
#' DID_CLIN, zp_fabian = "auf_in_d-1_in_d0")
#' erg
#' }
HalmScore <- function (FRM_B24,FRM_BEF, FRM_RR, FRM_O2A, FRM_O2P,FRM_BEAT,
                       DID_CLIN,zp_fabian="auf_in_d-1_in_d0") {
  # due to non-standard evaluation notes in R CMD check
  apo2.min_auf <- `apo2.min_d-1` <- apo2.min_d0 <- o2p.min_auf <-
    `o2p.min_d-1` <- o2p.min_d0 <- patstuid <- NULL
  #Halm-Score, urspruenglich von Katrin eingebaut am 14. Maerz 2017
  if ( !(zp_fabian %in% c("auf_in_d-1_in_d0")) ) {
    stop("ERROR: variable zp_fabian must be set to auf_in_d-1_in_d0
         It's not possible to calculate it for another time point!")
  } # Nach absprache mit peter 4.6.19 auffuellen nicht konsequent
  # auf_in_d-1_in_d0, sondern nur dort, wo es katrin gemacht hat,
  # um vergleichbarkeit basispaper zu optimieren

  toadd_hfrq.min =  getData4hfrqMin(FRM_B24,FRM_BEF)
  toadd_hfrq.max =  getData4hfrqMax(FRM_B24,FRM_BEF)
  toadd_sysbp.min = getData4sysbp.min (FRM_RR)
  toadd_afrq.max =  getData4afrqMax(FRM_B24,FRM_BEF)
  toadd_afrq.min =getData4afrqMin(FRM_B24,FRM_BEF)
  toadd_apo2.min = getData4apo2.min (FRM_O2A)
  toadd_o2p.min = getData4o2p.min (FRM_O2P)
  toadd_beat =getData4beat(FRM_BEAT)
  toadd_sauerst = getData4sauerst(FRM_O2A , FRM_O2P)
  toadd_temp=getData4temp(FRM_BEF, FRM_B24)
  toadd_verwirrt =   getData4verwirrt(FRM_BEF)
  toadd_gcs =  getData4gcs (DID_CLIN)

  DAT = merge(toadd_hfrq.min, toadd_hfrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_sysbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_apo2.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_o2p.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_beat, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_sauerst, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_temp, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_gcs, by= "patstuid", all = T,sort = F)

  # stopifnot(nrow(DAT[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(DAT[, patstuid]) == 0)
  setDF(DAT)

  #Herzfrequenz > 100 Schlaege -> ok
  hrf<-pmax(DAT$hfrq.min_d0,DAT$hfrq.max_d0,na.rm=T)
  myFilt<-is.na(hrf)
  dummy<-pmax(DAT$hfrq.min_auf,DAT$hfrq.max_auf,na.rm=T)
  hrf[myFilt]<-dummy[myFilt]
  HRF.p<-as.numeric(hrf>100)

  #systolischer Blutdruck < 90 mmHG -> ok
  sbp<-DAT$sysbp.min_d0
  myFilt<-is.na(sbp)
  sbp[myFilt]<-DAT$sysbp.min_auf[myFilt]
  SBP.p<-as.numeric(sbp<90)

  #Atemfrequenz > 24/min -> ok
  af<-pmax(DAT$afrq.min_d0,DAT$afrq.max_d0,na.rm=T)
  myFilt<-is.na(af)
  dummy<-pmax(DAT$afrq.min_auf,DAT$afrq.max_auf,na.rm=T)
  af[myFilt]<-dummy[myFilt]
  AF.p<-as.numeric(af>24)

  #Sauerstoffsaettigung ->
  setDT(DAT)
  minPoxy = DAT[, ifelse(is.na(o2p.min_d0)==F, o2p.min_d0,
                         ifelse(is.na(`o2p.min_d-1`)==F, `o2p.min_d-1`,
                                o2p.min_auf))]
  minApo2= DAT[,ifelse(is.na(apo2.min_d0)==F, apo2.min_d0,
                       ifelse(is.na(`apo2.min_d-1`)==F, `apo2.min_d-1`,
                              apo2.min_auf))]
  setDF(DAT)

  cutoff<-(minPoxy<90 | (minApo2 < 60))
  #Beatmung ? -> kein NA
  beatmet<-DAT$bea.d0
  #zusaetzlicher O2 ?
  extraO2<-DAT$sauerst_d0
  myFilt<-is.na(extraO2)
  extraO2[myFilt]<-DAT$sauerst_auf[myFilt]
  #Gesamtpunkt
  O2.p<-as.numeric(cutoff | beatmet | extraO2)

  #Koerpertemperatur > 37.8C -> ok
  kt<-pmax(DAT$temp.min_d0,DAT$temp.max_d0,na.rm=T)
  myFilt<-is.na(kt)
  dummy<-pmax(DAT$temp.min_auf,DAT$temp.max_auf,na.rm=T)
  kt[myFilt]<-dummy[myFilt]
  KT.p<-as.numeric(kt>37.8)

  #Mental Status verwirrt oder Glasgow Coma Scale < 15 -> ok
  verwirrt<-DAT$verwirrt_d0
  myFilt<-is.na(verwirrt)
  verwirrt[myFilt]<-DAT$verwirrt_auf[myFilt]
  verwirrt[is.na(verwirrt)]<-0
  MS.p<-as.numeric(verwirrt| (DAT$gcs_d0<15))

  #Gesamtscore berechnen
  dummy<-cbind(HRF.p,SBP.p,AF.p,O2.p,KT.p,MS.p)
  halm<-apply(dummy,1,function(x) sum(x,na.rm=T))


  res = data.table(halm)
  res$PATSTUID  = DAT$patstuid
  res$event = zeitpunkt2event(zp_fabian)
  # 2020-03-04 MRos: replace call to moveColFront for no dependency on toolboxH
  # res = moveColFront(res,c( "PATSTUID", 'event'))
  res <- data.table::setcolorder(res, neworder = c( "PATSTUID", 'event'))
  erg = c()
  erg$input  = DAT
  erg$input2 = c()

  erg$out  = res
  erg


  #completeness of score
  # dummy<-cbind(!is.na(HRF.p),!is.na(SBP.p),!is.na(AF.p),!is.na(O2.p),
  # !is.na(KT.p),!is.na(MS.p))
  # com<-apply(dummy,1,function(x) sum(x)/6)
  # sum(com>=0.5)
  # sum(com>=0.5)/1532
  # sum(com>=0.75)
  # sum(com>=0.75)/1532
  # sum(com>=0.9)
  # sum(com>=0.9)/1532
  # sum(com>=1)
  # sum(com>=1)/1532
}
