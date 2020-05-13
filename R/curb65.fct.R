#' Compute the CURB65 score.
#'
#' @param DID_PROBAND data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_DIL_LABORWERTE data.table containing the table with the same
#'  name from the database of the PROGRESS study
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref.
#' @param event2zeitpunkt_df data.table event2zeitpunkt_table (available with
#' the package).
#'
#' @return a named list with components: input and out. input is a data.table
#' in the wide format (one row per patient), containing the data used for
#' computing the CURB65 score. out is a data.table with one row
#' corresponding to one patient, identified by the
#' PATSTUID. The column curb contains the value of CURB65 score.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, 'FRM_DIL_LABORWERTE')
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR', guess_max = 10e5)
#' data.table::setDT(DID_PROBAND)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(FRM_RR)
#' erg <- curb65.fct(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE, FRM_RR,
#' zp_fabian = "d0")
#' erg
#' }
curb65.fct<- function(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                      FRM_RR, zp_fabian = "d0",
                      event2zeitpunkt_df =
                        progressdatenbankderivate::event2zeitpunkt_table){
  # due to non-standard evaluation notes in R CMD check
  verwirrt <- bun <- afrq.max <- sysbp.min <- diasbp.min <- NULL

  # if (!(time %in% c("auf","d0","d1","d2","d3","d4","auf_in_d0","d0_in_auf",
  #                   "auf+d0"))){
  #   stop("ERROR: time needs to equal one these values:
  #         auf, d0, d1, d2, d3, d4,
  #         auf_in_d0, d0_in_auf, d0+auf")
  # }

  if (!(zp_fabian %in% event2zeitpunkt_df$zp_fabianref)){
    stop("ERROR: zp_fabian needs to equal one these values: ",
         paste(event2zeitpunkt_df$zp_fabianref, collapse = ", "))
  }

  toadd_agesex <- getData4age.sex(DID_PROBAND)
  toadd_verwirrt <- getData4verwirrt(FRM_BEF)

  # BUN bzw SHARN   BUN x 2,143 = Serum Harnstoff
  toadd_bun <- getData4bun(FRM_DIL_LABORWERTE)

  # Atemfrequenz
  toadd_afrq.max <- getData4afrqMax(FRM_B24, FRM_BEF)

  # Systolischer Blutdruck (in mmHG)
  toadd_sysbp.min <- getData4sysbp.min(FRM_RR)

  # Diastolischer Blutdruck (in mmHG)
  toadd_diasbp.min <- getData4diasbp.min(FRM_RR)

  # zusammenbauen DAT
  DAT = merge(toadd_agesex, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_bun, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_sysbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_diasbp.min, by= "patstuid", all = T,sort = F)

  # generic
  age           <-DAT$age

  # time dependent
  variables<- c("verwirrt","bun","afrq.max","sysbp.min","diasbp.min")
  N<-dim(DAT)[1]
  if (zp_fabian %in% c("auf","d0","d1","d2","d3","d4")){
    for (i in 1:length(variables)){
      col_in_DAT <- paste(variables[i], zp_fabian, sep="_")
      if (col_in_DAT %in% colnames(DAT)){
        assign(variables[i], DAT[[col_in_DAT]] )
      } else {
        assign(variables[i], rep(NA,N) )
      }
    }
  } else {
    for (i in 1:length(variables)){
      col1_in_DAT <- paste(variables[i],"auf",sep="_")
      col2_in_DAT <- paste(variables[i],"d0",sep="_")
      if (col1_in_DAT %in% colnames(DAT)){
        dum1 <- DAT[[col1_in_DAT]]
      } else {
        dum1 <- rep(NA,N)
      }
      if (col2_in_DAT %in% colnames(DAT)){
        dum2 <- DAT[[col2_in_DAT]]
      } else {
        dum2 <- rep(NA,N)
      }
      if (zp_fabian=="auf_in_d0"){
        dum2[is.na(dum2)] <- dum1[is.na(dum2)]
        dum<-dum2
      }
      if (zp_fabian=="d0_in_auf"){
        dum1[is.na(dum1)] <- dum2[is.na(dum1)]
        dum<-dum1
      }
      if (zp_fabian=="auf+d0"){
        dum<-worst.value(dum1,dum2,variables[i])
      }
      assign(variables[i], dum )
    }
  }
  curbi<-curb65(verwirrt,bun,afrq.max,sysbp.min,diasbp.min,age) #[,c(2,1)]

  nas  <- 6 - c(is.na(verwirrt) +
                  is.na(bun) +
                  is.na(afrq.max) +
                  is.na(sysbp.min) +
                  is.na(diasbp.min) +
                  is.na(age))
  input <- data.table(PATSTUID = DAT$patstuid,
                      EVENT = zeitpunkt2event(zp_fabian),
                      verwirrt, bun, afrq.max,
                      sysbp.min, diasbp.min, age)
  curbi <- data.table(PATSTUID = DAT$patstuid,
                      EVENT = zeitpunkt2event(zp_fabian),
                      curbi, vollstaendig.aus.6=nas)
  # erg <- list(input = input, out = curbi)
  erg <- list(out = cbind(curbi, input[, !c("PATSTUID", "EVENT")]))
}


curb65<-function(verwirrt,bun,afrq.max,sysbp.min,diasbp.min,age){

  #Filter
  filt<- !(is.na(verwirrt) | is.na(bun) | is.na(afrq.max) | is.na(sysbp.min) |
             is.na(diasbp.min))
  # is_na_df <- data.frame(is_na_verwirrt = is.na(verwirrt),
  #                        is_na_bun = is.na(bun),
  #                        is_na_afrq.max = is.na(afrq.max),
  #                        is_na_sysbp.min = is.na(sysbp.min),
  #                        is_na_diasbp.min = is.na(diasbp.min),
  #                        is_na_age = is.na(age))

  #Auffuellen
  verwirrt[is.na(verwirrt)]    <- 0
  bun[is.na(bun)]              <- 6
  afrq.max[is.na(afrq.max)]    <- 20
  sysbp.min[is.na(sysbp.min)]  <- 120
  diasbp.min[is.na(diasbp.min)]  <- 80
  age[is.na(age)]              <- 60

  N    <- length(verwirrt)
  curb <- rep(NA,N)

  # curb65 fuer jede Person berechnen
  for (i in 1:N){
    count<-0
    if(verwirrt[i] == 1){
      count<-count+1
    }
    if(bun[i]>7){
      count<-count+1
    }
    if(afrq.max[i]>=30){
      count<-count+1
    }
    if( (sysbp.min[i]<90) | (diasbp.min[i]<=60) ){
      count<-count+1
    }
    if(age[i]>=65){
      count<-count+1
    }
    curb[i]<-count
  }

  curb.vollst        <- curb
  curb.vollst[!filt] <- NA

  curb_components <-
    data.frame(verwirrt_curb65 = as.numeric(verwirrt == 1),
               bun_curb65 = as.numeric(bun > 7),
               afrq.max_curb65 = as.numeric(afrq.max >= 30),
               bp_curb65 = as.numeric(sysbp.min < 90 | diasbp.min <= 60),
               age_curb65 = as.numeric(age >= 65))

  # return(cbind(curb.vollst,curb, curb_components, is_na_df))
  return(cbind(curb.vollst,curb, curb_components))
}
