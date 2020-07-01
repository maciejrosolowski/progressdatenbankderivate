#' Compute the CRB, CRB65, CURB and CURB65 score.
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
#' @param FRM_BEAT data.table containing the table with the same name from
#'  the database of the PROGRESS study. If it is NULL then the mechanical
#'  ventilation is not taken into account while computing the respiratory
#'  component of CURB65 which is as in the original definition of CURB65.
#'  Otherwise, if FRM_BEAT is non-NULL ventilated patients
#'  (those with the variable PATBEATM == 1 in the table FRM_BEAT) get 1 point.
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref.
#' @param event2zeitpunkt_df data.table event2zeitpunkt_table (available with
#' the package).
#'
#' @return a named list with one component: out. It is a data.table with one
#' row corresponding to one patient, identified by the
#' PATSTUID. The column crb, crb65, curb, curb65 contain the values of the
#' scores. Columns PATSTUID and EVENT identify the patient and time point.
#' Other columns contain data used for computing the components of the CURB65
#' scores.
#' If more than 50% of the subscores of each score are avaiable then the
#  corresponding score is available. Otherwise it is NA.
#' Note: If FRM_BEAT != NULL the calculation of the respiratory component of
#' the CURB65 score differs from the original definition of CURB65. Patients
#' who are ventilated are counted as if they their respiratory rate >= 30.
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
#' FRM_BEAT <- readxl::read_excel(excel_fn, 'FRM_BEAT', guess_max = 10e5)
#' data.table::setDT(DID_PROBAND)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_BEAT)
#' erg <- curb65.fct(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE, FRM_RR,
#' FRM_BEAT = NULL, zp_fabian = "d0")
#' erg
#' erg_bea <- curb65.fct(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
#' FRM_RR, FRM_BEAT, zp_fabian = "d0")
#' erg_bea
#' }
curb65.fct<- function(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                      FRM_RR, FRM_BEAT = NULL, zp_fabian = "d0",
                      event2zeitpunkt_df =
                        progressdatenbankderivate::event2zeitpunkt_table){
  if (is.null(FRM_BEAT)) {
    curb65.fct_default(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                       FRM_RR, zp_fabian, event2zeitpunkt_df)
  } else {
    # take mechanical ventilation into account while computing the respiratory
    # component of CURB65
    curb65.fct_bea(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                   FRM_RR, FRM_BEAT, zp_fabian, event2zeitpunkt_df)
  }
}


curb65.fct_default<- function(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                      FRM_RR, zp_fabian = "d0",
                      event2zeitpunkt_df =
                        progressdatenbankderivate::event2zeitpunkt_table){
  # due to non-standard evaluation notes in R CMD check
  verwirrt <- bun <- afrq.max <- sysbp.min <- diasbp.min <- NULL

  if (!(zp_fabian %in% c("auf","d0","d1","d2","d3","d4","auf_in_d0","d0_in_auf",
                    "auf+d0"))){
    stop("ERROR: zp_fabian needs to equal one these values:
          auf, d0, d1, d2, d3, d4,
          auf_in_d0, d0_in_auf, d0+auf")
  }

  # if (!(zp_fabian %in% event2zeitpunkt_df$zp_fabianref)){
  #   stop("ERROR: zp_fabian needs to equal one these values: ",
  #        paste(event2zeitpunkt_df$zp_fabianref, collapse = ", "))
  # }

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
  curbi<-curb65_default(verwirrt,bun,afrq.max,sysbp.min,diasbp.min,age)
  #[,c(2,1)]

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


curb65_default<-function(verwirrt,bun,afrq.max,sysbp.min,diasbp.min,age){

  # due to non-standard evaluation notes in R CMD check
  crb <- crb65 <- curb <- curb65 <- NULL

  #Filter
  filt<- !(is.na(verwirrt) | is.na(bun) | is.na(afrq.max) | is.na(sysbp.min) |
             is.na(diasbp.min))
  # is_na_df <- data.frame(is_na_verwirrt = is.na(verwirrt),
  #                        is_na_bun = is.na(bun),
  #                        is_na_afrq.max = is.na(afrq.max),
  #                        is_na_sysbp.min = is.na(sysbp.min),
  #                        is_na_diasbp.min = is.na(diasbp.min),
  #                        is_na_age = is.na(age))

  # #Auffuellen
  # verwirrt[is.na(verwirrt)]    <- 0
  # bun[is.na(bun)]              <- 6
  # afrq.max[is.na(afrq.max)]    <- 20
  # sysbp.min[is.na(sysbp.min)]  <- 120
  # diasbp.min[is.na(diasbp.min)]  <- 80
  # age[is.na(age)]              <- 60
  # patbea[is.na(patbea)] <- FALSE

  # N    <- length(verwirrt)
  # curb <- rep(NA,N)
  #
  # # curb65 fuer jede Person berechnen
  # for (i in 1:N){
  #   count<-0
  #   if(verwirrt[i] == 1){
  #     count<-count+1
  #   }
  #   if(bun[i]>7){
  #     count<-count+1
  #   }
  #   # if ventilated then 1 point. If not ventilated or ventilation unknown
  #   # then the respiratory rate determines whether the point is to be given
  #   if(afrq.max[i]>=30 | (patbea[i] & !is.na(patbea[i]))){
  #     count<-count+1
  #   }
  #   if( (sysbp.min[i]<90) | (diasbp.min[i]<=60) ){
  #     count<-count+1
  #   }
  #   if(age[i]>=65){
  #     count<-count+1
  #   }
  #   curb[i]<-count
  # }
  #
  # curb.vollst        <- curb
  # curb.vollst[!filt] <- NA

  curb_components <-
    data.table(verwirrt_curb65 = as.numeric(verwirrt == 1),
               bun_curb65 = as.numeric(bun > 7),
               afrq.max_curb65 = as.numeric(afrq.max >= 30),
               bp_curb65 = as.numeric(sysbp.min < 90 | diasbp.min <= 60),
               age_curb65 = as.numeric(age >= 65))

  # If more than 50% of the subscores of each score are avaiable then the
  # corresponding score is available. Otherwise it is NA.
  curb_components[, crb :=
                    ifelse(rowSums(is.na(.SD)) >= 2,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65",
                              "afrq.max_curb65", "bp_curb65")]
  curb_components[, crb65 :=
                    ifelse(rowSums(is.na(.SD)) >= 2,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65",
                              "afrq.max_curb65", "bp_curb65",
                              "age_curb65")]
  curb_components[, curb :=
                    ifelse(rowSums(is.na(.SD)) >= 2,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65", "bun_curb65",
                              "afrq.max_curb65", "bp_curb65")]
  curb_components[, curb65 :=
                    ifelse(rowSums(is.na(.SD)) >= 3,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65", "bun_curb65",
                              "afrq.max_curb65", "bp_curb65",
                              "age_curb65")]

  # # if all components of the scores are NA then set the scores to 0
  # # uncomment it, if they should be NA instead
  # curb_components[is.na(crb), crb := 0]
  # curb_components[is.na(crb65), crb65 := 0]
  # curb_components[is.na(curb), curb := 0]
  # curb_components[is.na(curb65), curb65 := 0]

  # return(cbind(curb.vollst,curb, curb_components, is_na_df))
  # return(cbind(curb.vollst,curb, curb_components))
  return(curb_components)
}




curb65.fct_bea<- function(DID_PROBAND, FRM_BEF, FRM_B24, FRM_DIL_LABORWERTE,
                      FRM_RR, FRM_BEAT, zp_fabian = "d0",
                      event2zeitpunkt_df =
                        progressdatenbankderivate::event2zeitpunkt_table){
  # due to non-standard evaluation notes in R CMD check
  verwirrt <- bun <- afrq.max <- sysbp.min <- diasbp.min <- patbea <- NULL

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

  # 2020-05-13 MRos: Beatmung
  # als Kriterium alternativ zur Atemfrequenz
  toadd_beat <- getData4beat(FRM_BEAT)

  # zusammenbauen DAT
  DAT = merge(toadd_agesex, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_bun, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_sysbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_diasbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_beat, by= "patstuid", all = T,sort = F)

  # generic
  age           <-DAT$age

  # time dependent
  variables<- c("verwirrt","bun","afrq.max","sysbp.min","diasbp.min", "patbea")
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
  curbi<-curb65_bea(verwirrt,bun,afrq.max,sysbp.min,diasbp.min,age, patbea)
  #[,c(2,1)]

  nas  <- 7 - c(is.na(verwirrt) +
                  is.na(bun) +
                  is.na(afrq.max) +
                  is.na(sysbp.min) +
                  is.na(diasbp.min) +
                  is.na(age) +
                  is.na(patbea))
  input <- data.table(PATSTUID = DAT$patstuid,
                      EVENT = zeitpunkt2event(zp_fabian),
                      verwirrt, bun, afrq.max,
                      sysbp.min, diasbp.min, age, patbea)
  curbi <- data.table(PATSTUID = DAT$patstuid,
                      EVENT = zeitpunkt2event(zp_fabian),
                      curbi, vollstaendig.aus.7=nas)
  # erg <- list(input = input, out = curbi)
  erg <- list(out = cbind(curbi, input[, !c("PATSTUID", "EVENT")]))
}


curb65_bea<-function(verwirrt,bun,afrq.max,sysbp.min,diasbp.min,age, patbea){

  # due to non-standard evaluation notes in R CMD check
  crb <- crb65 <- curb <- curb65 <- NULL

  #Filter
  filt<- !(is.na(verwirrt) | is.na(bun) | is.na(afrq.max) | is.na(sysbp.min) |
             is.na(diasbp.min) | is.na(patbea))
  # is_na_df <- data.frame(is_na_verwirrt = is.na(verwirrt),
  #                        is_na_bun = is.na(bun),
  #                        is_na_afrq.max = is.na(afrq.max),
  #                        is_na_sysbp.min = is.na(sysbp.min),
  #                        is_na_diasbp.min = is.na(diasbp.min),
  #                        is_na_age = is.na(age))

  # #Auffuellen
  # verwirrt[is.na(verwirrt)]    <- 0
  # bun[is.na(bun)]              <- 6
  # afrq.max[is.na(afrq.max)]    <- 20
  # sysbp.min[is.na(sysbp.min)]  <- 120
  # diasbp.min[is.na(diasbp.min)]  <- 80
  # age[is.na(age)]              <- 60
  # patbea[is.na(patbea)] <- FALSE

  # N    <- length(verwirrt)
  # curb <- rep(NA,N)
  #
  # # curb65 fuer jede Person berechnen
  # for (i in 1:N){
  #   count<-0
  #   if(verwirrt[i] == 1){
  #     count<-count+1
  #   }
  #   if(bun[i]>7){
  #     count<-count+1
  #   }
  #   # if ventilated then 1 point. If not ventilated or ventilation unknown
  #   # then the respiratory rate determines whether the point is to be given
  #   if(afrq.max[i]>=30 | (patbea[i] & !is.na(patbea[i]))){
  #     count<-count+1
  #   }
  #   if( (sysbp.min[i]<90) | (diasbp.min[i]<=60) ){
  #     count<-count+1
  #   }
  #   if(age[i]>=65){
  #     count<-count+1
  #   }
  #   curb[i]<-count
  # }
  #
  # curb.vollst        <- curb
  # curb.vollst[!filt] <- NA

  curb_components <-
    data.table(verwirrt_curb65 = as.numeric(verwirrt == 1),
               bun_curb65 = as.numeric(bun > 7),
               afrq.max_curb65 = as.numeric(afrq.max >= 30 |
                                              (patbea & !is.na(patbea))),
               bp_curb65 = as.numeric(sysbp.min < 90 | diasbp.min <= 60),
               age_curb65 = as.numeric(age >= 65))

  # If more than 50% of the subscores of each score are avaiable then the
  # corresponding score is available. Otherwise it is NA.
  curb_components[, crb :=
                    ifelse(rowSums(is.na(.SD)) >= 2,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65",
                              "afrq.max_curb65", "bp_curb65")]
  curb_components[, crb65 :=
                    ifelse(rowSums(is.na(.SD)) >= 2,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65",
                              "afrq.max_curb65", "bp_curb65",
                              "age_curb65")]
  curb_components[, curb :=
                    ifelse(rowSums(is.na(.SD)) >= 2,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65", "bun_curb65",
                              "afrq.max_curb65", "bp_curb65")]
  curb_components[, curb65 :=
                    ifelse(rowSums(is.na(.SD)) >= 3,
                           NA_integer_, rowSums(.SD, na.rm = TRUE)),
                  .SDcols = c("verwirrt_curb65", "bun_curb65",
                              "afrq.max_curb65", "bp_curb65",
                              "age_curb65")]

  # # if all components of the scores are NA then set the scores to 0
  # # uncomment it, if they should be NA instead
  # curb_components[is.na(crb), crb := 0]
  # curb_components[is.na(crb65), crb65 := 0]
  # curb_components[is.na(curb), curb := 0]
  # curb_components[is.na(curb65), curb65 := 0]

  # return(cbind(curb.vollst,curb, curb_components, is_na_df))
  # return(cbind(curb.vollst,curb, curb_components))
  return(curb_components)
}
