#' Compute the SIRS score.
#'
#' @param DID_PROBAND data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BAS data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_DIL_LABORWERTE data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param DID_CLIN data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_KAT data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref.
#' @param event2zeitpunkt_df data.table event2zeitpunkt_table (available with
#' the package).
#'
#' @return a named list with components: input, input2 and out. out is
#' a data.table with one row corresponding to a combination of PATSTUID
#' (patient) and "EVENT" (time point). infec.septic.servsept contains
#' the number of the met SIRS criteria (infected (1), sepsis (2),
#' severe sepsis(3)); septischer.schock indicates if criteria for septic shock
#' are met. If 50% or less of the 16 input parameters are NA then the score
#' is set to NA.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
#' FRM_BAS <- readxl::read_excel(excel_fn, 'FRM_BAS')
#' FRM_BEF <- readxl::read_excel(excel_fn, 'FRM_BEF')
#' FRM_B24 <- readxl::read_excel(excel_fn, 'FRM_B24')
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' DID_CLIN <- readxl::read_excel(excel_fn, 'DID_CLIN')
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR', guess_max = 10e5)
#' FRM_KAT <- readxl::read_excel(excel_fn, 'FRM_KAT')
#' data.table::setDT(DID_PROBAND)
#' data.table::setDT(FRM_BAS)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(DID_CLIN)
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_KAT)
#' erg_d0 <- sirs.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24,
#' FRM_DIL_LABORWERTE, DID_CLIN, FRM_RR, FRM_KAT, zp_fabian = "d0")
#' erg_d0
#' erg_d1 <- sirs.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24,
#' FRM_DIL_LABORWERTE, DID_CLIN, FRM_RR, FRM_KAT, zp_fabian = "d1")
#' erg_d1
#' }
sirs.fct <- function(DID_PROBAND,FRM_BAS, FRM_BEF, FRM_B24,FRM_DIL_LABORWERTE,
                     DID_CLIN,FRM_RR,FRM_KAT,zp_fabian="d0",
                     event2zeitpunkt_df =
                       progressdatenbankderivate::event2zeitpunkt_table){
  # due to non-standard evaluation notes in R CMD check
  EVENTKombination <- PATSTUID <- WEIGHT <- afrq.max <- bemin <- diur <-
    hfrq.max <- kate <- leuko_max <- leuko_min <- map <- oxi.ind <- patstuid <-
    age <- pco2 <- smkern.neutro <- stkern.neutro <- sysbp.min <- temp.max <-
    temp.min <- thrombo_min <- verwirrt <- zp_fabian1DayBefore <-
    zp_fabianref <- infec.septic.servsept <- septischer.schock <-
    vollstaendig.aus.16 <- NULL

  toadd_gewicht = DID_PROBAND[,.(patstuid =PATSTUID, gewicht=WEIGHT)]
  toadd_chr.lunge = getData4chr.lunge(FRM_BAS)
  toadd_temp=getData4temp(FRM_BEF, FRM_B24)
  toadd_hfrq.max =  getData4hfrqMax(FRM_B24, FRM_BEF)
  toadd_afrq.max=getData4afrqMax (FRM_B24, FRM_BEF)
  toadd_pco2.max= getData4pco2(FRM_B24)
  toadd_leuko.total = getData4leuko(FRM_DIL_LABORWERTE)
  toadd_smkern.neutro = getData4smkern.neutro(FRM_DIL_LABORWERTE)
  toadd_stkern.neutro = getData4stkern.neutro(FRM_DIL_LABORWERTE)
  toadd_verwirrt =   getData4verwirrt(FRM_BEF)
  toadd_thrombo.total =getData4thrombo(FRM_DIL_LABORWERTE,DID_CLIN )
  toadd_oxi.ind = getData4oxi.ind(DID_CLIN)
  toadd_diurValue= getData4diurValue(FRM_B24)
  toadd_bemin=getData4bemin (FRM_B24)
  toadd_sysbp.min = getData4sysbp.min (FRM_RR)
  toadd_map=getData4map (DID_CLIN)
  toadd_kate = getData4kate (FRM_KAT)

  # ## Zusammenbauen
  DAT = merge(toadd_gewicht, toadd_chr.lunge, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_temp, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_hfrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_pco2.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_leuko.total, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_smkern.neutro, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_stkern.neutro, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_thrombo.total, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_oxi.ind, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_diurValue, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_bemin, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_sysbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_map, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_kate, by= "patstuid", all = T,sort = F)

  # showNA(DAT)
  # stopifnot(nrow(DAT[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(DAT[, patstuid]) == 0)
  setDF(DAT)



  # generic
  chr.lunge     <-DAT$chr.lunge
  gewicht       <-DAT$gewicht

  # zp_fabian dependent
  variables<- c("temp.min","temp.max","hfrq.max","afrq.max","pco2","leuko_min",
                "leuko_max","stkern.neutro","smkern.neutro","verwirrt",
                "thrombo_min","oxi.ind","diur","bemin","sysbp.min","map",
                "kate")
  N<-dim(DAT)[1]
  #  for (i in 1:length(variables)){
  #    col_in_DAT <- paste(variables[i],zp_fabian,sep="_")
  #    if (col_in_DAT %in% colnames(DAT)){
  #      assign(variables[i], DAT[,col_in_DAT] )
  #    } else {
  #      assign(variables[i], rep(NA,N) )
  #    }
  #  }

  ####
  # 2020-03-03 MRos, comment: if zp_fabian is not a combination of two time
  # points but a single point in time then just assign the variables from
  # DAT from that time point to global variables.
  if (zp_fabian %in% event2zeitpunkt_df[EVENTKombination==F,
                                        na.omit(zp_fabianref)]){
    # war bei fabian nur c("auf","d0","d1","d2","d3","d4")
    for (i in 1:length(variables)){
      col_in_DAT <- paste(variables[i],zp_fabian,sep="_")
      if (col_in_DAT %in% colnames(DAT)){
        assign(variables[i], DAT[,col_in_DAT] )
      } else {
        message("Variable:", variables[i], " not available, replacing with NAs.")
        assign(variables[i], rep(NA,N) )
      }
    }
  } else {
    # 2020-03-03 MRos, comment: if zp_fabian is an aggregate of two time points
    # then compute the aggregated variables and assign them to global variables
    for (i in 1:length(variables)){
      col1_in_DAT <- paste(variables[i],"auf",sep="_")
      col2_in_DAT <- paste(variables[i],"d0",sep="_")
      if (col1_in_DAT %in% colnames(DAT)){
        dum1 <- DAT[,col1_in_DAT]
      } else {
        dum1 <- rep(NA,N)
      }
      if (col2_in_DAT %in% colnames(DAT)){
        dum2 <- DAT[,col2_in_DAT]
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
        if (variables[i] %in% c("stkern.neutro","smkern.neutro")){
          dum<-DAT[,paste(variables[i],"d0",sep="_")]
        } else {
          dum<-worst.value(dum1,dum2,variables[i])
        }
      }
      assign(variables[i], dum )
    }
  }

  ####

  if (zp_fabian %in%  event2zeitpunkt_df[is.na(zp_fabian1DayBefore)==T,
                                            zp_fabianref]){
    # war bei fabian c("auf","d0","d0_in_auf","auf_in_d0","auf+d0")
    thrombo.daybefore <- rep(NA,N)
  }
  day<- event2zeitpunkt_df[is.na(zp_fabian1DayBefore)==F, zp_fabianref]
  # war bei fabian c("d1","d2","d3","d4")
  if (zp_fabian %in% day){
    # day = "d1" # debug
    # 2020-03-03 MRos: proposed modifications:
    # 1. change match_hk() to match() to avoid the dependency on the toolboxH
    # package.
    # 2. change match(day...) to match(zp_fabian...). Otherwise daybef is a
    # vector of several names which is probably not intended. This is no
    # problem, if zp_fabian == "d0" because (zp_fabian %in% day) == FALSE and
    # thrombo.daybefore is assigned a vector of NAs.
    # But if zp_fabian == "d1" then thrombo.daybefore is a data.table with
    # several columns and it causes an error later on in the function sirs.III.
    # I compared the results for zp_fabian == "d0" and for zp_fabian == "d1" to
    # the scores computed earlier:
    # load("/net/ifs1/san_projekte/projekte/
    # genstat/02_projekte/1208_progress/Basispaper/Analysen/
    # 03_score_calculation_Katrin_CHECK_hk2.RData")
    # and the results agreed almost completely and equally well for d0 and d1.
    # daybef<- event2zeitpunkt_df[
    #   match_hk(day, event2zeitpunkt_df$zp_fabianref), zp_fabian1DayBefore]
    daybef<- event2zeitpunkt_df[
      match(zp_fabian, event2zeitpunkt_df$zp_fabianref), zp_fabian1DayBefore]
    # war bei fabianpaste("d",which(zp_fabian==day)-1,sep="")
    col_in_DAT <- paste("thrombo_min",daybef,sep="_")
    if (col_in_DAT %in% colnames(DAT)){
      assign("thrombo.daybefore", DAT[,col_in_DAT] )
    } else {
      assign("thrombo.daybefore", rep(NA,N) )
    }
  }

  # NAs zaehlen
  nas<- !(is.na(temp.min) |
            is.na(temp.max) |
            is.na(hfrq.max) |
            is.na(afrq.max) |
            is.na(pco2) |
            is.na(leuko_min) |
            is.na(leuko_max) |
            is.na(stkern.neutro) |
            is.na(smkern.neutro) |
            is.na(verwirrt) |
            is.na(thrombo_min) |
            is.na(thrombo.daybefore) |
            is.na(oxi.ind) |
            is.na(chr.lunge) |
            is.na(diur) |
            is.na(gewicht) |
            is.na(bemin) |
            is.na(sysbp.min) |
            is.na(map) |
            is.na(kate)
  )
  nas2<-  16-(is.na(temp.min) +
                is.na(hfrq.max) +
                is.na(afrq.max) +
                is.na(pco2) +
                is.na(leuko_min) +
                is.na(stkern.neutro) +
                is.na(smkern.neutro) +
                is.na(verwirrt) +
                is.na(thrombo_min) +
                is.na(thrombo.daybefore) +
                is.na(oxi.ind) +
                is.na(chr.lunge) +
                is.na(diur) +
                is.na(gewicht) +
                is.na(bemin) +
                is.na(sysbp.min)# +
              #            is.na(map) +               nur fuer sep. schock
              #            is.na(kate)                nur fuer sep. schock
  )
  nas3<-data.frame(na.temp=is.na(temp.min),
                   na.hfrq=is.na(hfrq.max),
                   na.afrq=is.na(afrq.max),
                   na.pco2=is.na(pco2),
                   na.leuko=is.na(leuko_min),
                   na.stkern.neutro=is.na(stkern.neutro) ,
                   na.smkern.neutro=is.na(smkern.neutro) ,
                   na.verwirrt=is.na(verwirrt) ,
                   na.thrombo_min=is.na(thrombo_min) ,
                   na.thrombo.daybefore=is.na(thrombo.daybefore) ,
                   na.oxi.ind=is.na(oxi.ind) ,
                   na.chr.lunge=is.na(chr.lunge) ,
                   na.diur=is.na(diur) ,
                   na.gewicht=is.na(gewicht) ,
                   na.bemin=is.na(bemin) ,
                   na.sysbp=is.na(sysbp.min)# ,
                   #          na.map=is.na(map) ,      nur fuer sep. schock
                   #          na.kate=is.na(kate)      nur fuer sep. schock
  )

  out<-sirs.day(temp.min,temp.max,hfrq.max,afrq.max,pco2,leuko_min,leuko_max,
                stkern.neutro,smkern.neutro,verwirrt,thrombo_min,
                thrombo.daybefore,oxi.ind, chr.lunge, diur, gewicht, bemin,
                sysbp.min,map,kate)
  overview<-rep(1,N)
  overview[out[[2]]$sepsis]<-2
  overview[out[[2]]$schwere.sepsis]<-3
  # par(mfrow=c(1,1))
  # barplot(t(table(overview,out[[2]]$septischer.schock)), col = c("grey","red"),
  #         main="SIRS-Scores",
  #         names.arg = c("infected","septic","severe\nseptic"),
  #         legend.text = c("no sep.shock","septic shock"))

  out<-data.frame(out[[2]] ,  infec.septic.servsept = overview,
                  vollstaendig=nas, vollstaendig.aus.16=nas2 )
  out<-out[,c(7,4,5,6,1,2,3,8,9)]
  out<-data.frame(out,nas3)

  out = data.table(out)
  out$PATSTUID  = DAT$patstuid
  out$EVENT = zeitpunkt2event(zp_fabian)
  # 2020-03-03 MRos: replace call to moveColFront for no dependency on toolboxH
  # out = moveColFront(out,c( "PATSTUID", 'event'))
  out <- data.table::setcolorder(out, neworder = c("PATSTUID", "EVENT"))
  # 2020-07-01 MRos: apply the 50% rule. If <= 50% subscores NA then score NA
  out[vollstaendig.aus.16 <= 8, infec.septic.servsept := NA]
  out[vollstaendig.aus.16 <= 8, septischer.schock := NA]
  erg = c()
  erg$input  = DAT
  erg$input2 = c()
  #list(DAT$patstuid, age,verwirrt,hfrq.max,afrq.max,sysbp.min,temp.min,
  #temp.max,tumor,herz,cerebro,renal,liver,
  # gender,nurse.home,art.ph.min,bun,snat,gluk,haemkrt,
  # apo2.min,pleu_erg)
  # names(erg$input2) = c('patstuid','age','verwirrt','hfrq.max','afrq.max',
  # 'sysbp.min','temp.min','temp.max','tumor','herz','cerebro','renal','liver',
  # 'gender','nurse.home','art.ph.min','bun','snat','gluk','haemkrt',
  # 'apo2.min','pleu_erg')

  erg$out  = out
  erg
}

#' Compute the SIRS score based on several parameters
#'
#' @param temp.min a variable created inside sirs.fct
#' @param temp.max a variable created inside sirs.fct
#' @param hfrq.max a variable created inside sirs.fct
#' @param afrq.max a variable created inside sirs.fct
#' @param pco2 a variable created inside sirs.fct
#' @param leuko_min a variable created inside sirs.fct
#' @param leuko_max a variable created inside sirs.fct
#' @param stekern.neutro a variable created inside sirs.fct
#' @param smkern.neutro a variable created inside sirs.fct
#' @param verwirrt a variable created inside sirs.fct
#' @param thrombo_min a variable created inside sirs.fct
#' @param thrombo.daybefore a variable created inside sirs.fct
#' @param oxi.ind a variable created inside sirs.fct
#' @param chr.lunge a variable created inside sirs.fct
#' @param diur a variable created inside sirs.fct
#' @param gewicht a variable created inside sirs.fct
#' @param bemin a variable created inside sirs.fct
#' @param sysbp.min a variable created inside sirs.fct
#' @param map a variable created inside sirs.fct
#' @param kate a variable created inside sirs.fct
#' @return a list with two components
#' @noRd
sirs.day<-function(temp.min,temp.max,hfrq.max,afrq.max,pco2,leuko_min,
                   leuko_max,stkern.neutro,smkern.neutro,verwirrt, thrombo_min,
                   thrombo.daybefore, oxi.ind, chr.lunge, diur, gewicht,
                   bemin,sysbp.min,map,kate){

  outsirsII  <- sirs.II(temp.min,temp.max,hfrq.max,afrq.max,pco2,leuko_min,
                        leuko_max,stkern.neutro,smkern.neutro)
  outsirsIII <- sirs.III(verwirrt, thrombo_min, thrombo.daybefore, oxi.ind,
                         chr.lunge, diur, gewicht, bemin)
  outschock  <- schock(sysbp.min,map,kate)

  out.vollst <-data.frame(krit.I=rep(1,length(temp.min)),
                          krit.II=outsirsII[,1],krit.III=outsirsIII[,1])
  out.ersetzt<-data.frame(krit.I=rep(1,length(temp.min)),
                          krit.II=outsirsII[,2],krit.III=outsirsIII[,2])
  # Sepsis: KritI & KritII,    Schwere Sepsis KritI & KritII & KritIII
  sepsis            <- (out.vollst$krit.I==1) & (out.vollst$krit.II>1)
  schwere.sepsis    <- (out.vollst$krit.I==1) & (out.vollst$krit.II>1) &
    (out.vollst$krit.III>0)
  septischer.schock <- (out.vollst$krit.I==1) & (out.vollst$krit.II>1) &
    (outschock[,1]>0)
  out.vollst        <- data.frame(out.vollst,sepsis,schwere.sepsis,
                                  septischer.schock)

  sepsis            <- (out.ersetzt$krit.I==1) & (out.ersetzt$krit.II>1)
  schwere.sepsis    <- (out.ersetzt$krit.I==1) & (out.ersetzt$krit.II>1) &
    (out.ersetzt$krit.III>0)
  septischer.schock <- (out.ersetzt$krit.I==1) & (out.ersetzt$krit.II>1) &
    (outschock[,2]>0)
  out.ersetzt       <- data.frame(out.ersetzt,sepsis,schwere.sepsis,
                                  septischer.schock)

  list(out.vollst,out.ersetzt)
}

#' Compute the SIRS criterion II (sepsis) based on several parameters
#'
#' @param temp.min a variable created inside sirs.fct
#' @param temp.max a variable created inside sirs.fct
#' @param hfrq.max a variable created inside sirs.fct
#' @param afrq.max a variable created inside sirs.fct
#' @param pco2 a variable created inside sirs.fct
#' @param leuko_min a variable created inside sirs.fct
#' @param leuko_max a variable created inside sirs.fct
#' @param stekern.neutro a variable created inside sirs.fct
#' @param smkern.neutro a variable created inside sirs.fct
#' @param verwirrt a variable created inside sirs.fct
#' @param thrombo_min a variable created inside sirs.fct
#' @param thrombo.daybefore a variable created inside sirs.fct
#' @param oxi.ind a variable created inside sirs.fct
#' @param chr.lunge a variable created inside sirs.fct
#' @param diur a variable created inside sirs.fct
#' @param gewicht a variable created inside sirs.fct
#' @param bemin a variable created inside sirs.fct
#' @param sysbp.min a variable created inside sirs.fct
#' @param map a variable created inside sirs.fct
#' @param kate a variable created inside sirs.fct
#' @return a list with two components
#' @noRd
sirs.II   <-function(temp.min,temp.max,hfrq.max,afrq.max,pco2,leuko_min,
                     leuko_max,stkern.neutro,smkern.neutro){
  filt<- !(is.na(temp.min) |
             is.na(temp.max) |
             is.na(hfrq.max) |
             is.na(afrq.max) |
             is.na(pco2) |
             is.na(leuko_min) |
             is.na(leuko_max) |
             is.na(stkern.neutro) |
             is.na(smkern.neutro))
  temp.min[is.na(temp.min)]           <-  37
  temp.max[is.na(temp.max)]           <-  37
  hfrq.max[is.na(hfrq.max)]           <-  80
  afrq.max[is.na(afrq.max)]           <-  15
  pco2[is.na(pco2)]                   <-  5
  leuko_min[is.na(leuko_min)]         <-  10
  leuko_max[is.na(leuko_max)]         <-  10
  stkern.neutro[is.na(stkern.neutro) | is.na(smkern.neutro)] <- 1
  # ergibt zusammen einen unkrit. anteil von 5 %
  smkern.neutro[is.na(stkern.neutro) | is.na(smkern.neutro)] <- 19
  # (stabkernige an allen neutrophilen)

  sum.krit<-c()
  for (i in 1:length(afrq.max)){
    count<-0
    if ((temp.min[i]<= 36) | (temp.max[i] >=38)){
      count<- count +1
    }
    if (hfrq.max[i] >= 90){
      count<- count +1
    }
    if ((afrq.max[i] >= 20) | (pco2[i] <= 4.3)){
      count<- count +1
    }
    if ((leuko_max[i] >= 12) | (leuko_min[i] <= 4) |
        ( (stkern.neutro[i]/(stkern.neutro[i]+smkern.neutro[i])) >= 0.1) ){
      count<- count +1
    }
    sum.krit[i]<-count
  }
  sum.krit.vollst<-sum.krit
  sum.krit.vollst[!filt] <- NA
  return(cbind(sum.krit.vollst,sum.krit))
}

#' Compute the SIRS criterion III (severe sepsis) based on several parameters
#'
#' @param verwirrt a variable created inside sirs.fct
#' @param thrombo_min a variable created inside sirs.fct
#' @param thrombo.daybefore a variable created inside sirs.fct
#' @param oxi.ind a variable created inside sirs.fct
#' @param chr.lunge a variable created inside sirs.fct
#' @param diur a variable created inside sirs.fct
#' @param gewicht a variable created inside sirs.fct
#' @param bemin a variable created inside sirs.fct
#' @return a list with two components
#' @noRd
sirs.III <- function(verwirrt, thrombo_min, thrombo.daybefore, oxi.ind,
                     chr.lunge, diur, gewicht, bemin){
  filt<- !(is.na(verwirrt) |
             is.na(thrombo_min) |
             is.na(oxi.ind) |
             is.na(chr.lunge) |
             is.na(diur) |
             is.na(gewicht) |
             is.na(bemin))
  verwirrt[is.na(verwirrt)]               <- 0
  thrombo.change                          <-
    (thrombo.daybefore-thrombo_min)/thrombo.daybefore
  thrombo.change[is.na(thrombo.change)]   <- 0
  thrombo_min[is.na(thrombo_min)]                 <- 200
  oxi.ind[is.na(oxi.ind)]                         <- 40
  chr.lunge[is.na(chr.lunge)]             <- 0
  rel.diur                                <- diur/24/gewicht
  rel.diur[is.na(rel.diur)]               <- 1
  bemin[is.na(bemin)]                     <- 0

  sum.krit<-c()
  for (i in 1:length(verwirrt)){
    count<-0
    if( verwirrt[i] == 1 ){
      count <- count +1
    }
    if( (thrombo_min[i]<= 100) | (thrombo.change[i]>0.3) ){
      count <- count +1
    }
    if( (oxi.ind[i]<=33) & (chr.lunge[i] == 0) ){
      count <- count +1
    }
    if( rel.diur[i] <= 0.5 ){
      count <- count +1
    }
    if( bemin[i] <= -5 ){
      count <- count +1
    }
    sum.krit[i]<-count
  }
  sum.krit.vollst<-sum.krit
  sum.krit.vollst[!filt] <- NA
  return(cbind(sum.krit.vollst,sum.krit))
}

#' Compute the septic shock criterion of SIRS based on several parameters
#'
#' @param sysbp.min a variable created inside sirs.fct
#' @param map a variable created inside sirs.fct
#' @param kate a variable created inside sirs.fct
#' @return a list with two components
#' @noRd
schock <- function(sysbp.min,map,kate){
  filt <- !(is.na(sysbp.min) & is.na(map) )
  # werten es nur als nicht vorhanden wenn keiner der Werte verfuegbar ist
  sysbp.min[is.na(sysbp.min)]    <-  120
  map[is.na(map)]          <-  10
  kate[is.na(kate)]    <-  0

  sum.krit<-c()
  for (i in 1:length(map)){
    count<-0
    if( (sysbp.min[i] <= 90) | (map[i] <=8.666) | (kate[i] == 1) ){
      count <- 1
    }
    sum.krit[i]<-count
  }
  sum.krit.vollst<-sum.krit
  sum.krit.vollst[!filt] <- NA
  return(cbind(sum.krit.vollst,sum.krit))
}
