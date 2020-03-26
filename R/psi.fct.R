#' Compute the PSI (pneumonia severity index).
#'
#' @param DID_PROBAND data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BAS data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_BEF data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_B24 data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_RR data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_O2A data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_DIL_LABORWERTE data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param FRM_VIS data.table containing the table with the same name from
#'  the database of the PROGRESS study
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref.
#' @param event2zeitpunkt_df data.table event2zeitpunkt_table (available with
#' the package).
#'
#' @return a named list with components: input, input2 and out. out is
#' a data.table with one row corresponding to a combination of PATSTUID
#' (patient) and "EVENT" (time point). Column psi.class contains the PSI
#' values computed by imputing non-critical values, if some components
#' of the score were missing. The column psi.class.fillt contains NAs, if
#' any of the components needed for computing the PSI score were missing.
#' The column vollstaendig.von.20 contains information about the number of
#' components having values available out of the overall 20 components
#' of the PSI score.
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
#' FRM_RR <- readxl::read_excel(excel_fn, 'FRM_RR')
#' FRM_O2A <- readxl::read_excel(excel_fn, 'FRM_O2A')
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' FRM_VIS <- readxl::read_excel(excel_fn, 'FRM_VIS')
#' data.table::setDT(DID_PROBAND)
#' data.table::setDT(FRM_BAS)
#' data.table::setDT(FRM_BEF)
#' data.table::setDT(FRM_B24)
#' data.table::setDT(FRM_RR)
#' data.table::setDT(FRM_O2A)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' data.table::setDT(FRM_VIS)
#' erg_d0 <- psi.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24, FRM_RR, FRM_O2A,
#' FRM_DIL_LABORWERTE,FRM_VIS, zp_fabian = "d0")
#' erg_d0
#' erg_d1 <- psi.fct(DID_PROBAND, FRM_BAS, FRM_BEF, FRM_B24, FRM_RR, FRM_O2A,
#' FRM_DIL_LABORWERTE,FRM_VIS, zp_fabian = "d1")
#' erg_d1
#' }
psi.fct <- function(DID_PROBAND,FRM_BAS, FRM_BEF, FRM_B24,FRM_RR,FRM_O2A,
                    FRM_DIL_LABORWERTE,FRM_VIS, zp_fabian="d0",
                    event2zeitpunkt_df =
                      progressdatenbankderivate::event2zeitpunkt_table){
  # due to non-standard evaluation notes in R CMD check
  EVENTKombination <- afrq.max <- gender <- gluk <- haemkrt <- herz <-
    hfrq.max <- liver <- nurse.home <- patstuid <- age <- apo2.min <-
    art.ph.min <- bun <- cerebro <- pleu_erg <- renal <- snat <- sysbp.min <-
    temp.max <- temp.min <- tumor <- verwirrt <- zp_fabianref <- NULL

  if (!(zp_fabian %in% event2zeitpunkt_df$zp_fabianref)){
    stop("ERROR: zp_fabian needs to equal one these values: ",
         paste(event2zeitpunkt_df$zp_fabianref, collapse = ", "))
  }

  toadd_agesex = getData4age.sex (DID_PROBAND)
  # tumor yerz cerebro renal liver nurse.home
  toadd_tum.herz.cer.ren.liv.nurs =  getData4tum.herz.cer.ren.liv.nurs (FRM_BAS)
  # verwirrt
  toadd_verwirrt =   getData4verwirrt(FRM_BEF)

  # 22 Herzfrequenz
  toadd_hfrq.max =  getData4hfrqMax(FRM_B24,FRM_BEF)

  # 23 Atemfrequenz
  toadd_afrq.max=getData4afrqMax(FRM_B24,FRM_BEF)

  # 24. Systolischer Blutdruck (in mmHG)
  toadd_sysbp.min = getData4sysbp.min (FRM_RR)

  # 26 Koerpertemperatur
  toadd_temp=getData4temp(FRM_BEF, FRM_B24)

  # 26. Arterial pH   (APHMIN; FRM-B24, FRM-O2A)  klein ist schlecht
  toadd_art.ph= getData4art.ph(FRM_B24, FRM_O2A)

  # 27 BUN bzw SHARN   BUN x 2,143 = Serum Harnstoff
  toadd_bun = getData4bun(FRM_DIL_LABORWERTE )

  # 28 SNAT
  toadd_snat =  getData4SNAT(FRM_DIL_LABORWERTE)

  # 29 Glukose
  toadd_gluk = getData4gluk(FRM_DIL_LABORWERTE)

  # 30 Haematorkrit
  toadd_haemkrt= getData4haemkrt(FRM_DIL_LABORWERTE)

  # 31 part pressure of arterial 02 (in mmHg) (APO2, FRM-O2A) klein ist schlecht
  toadd_apo2.min = getData4apo2.min (FRM_O2A)

  # 32 Pleural effusion,  (PLEUERGUSS, aus FRM_BEF und FRM VIS)
  ### Fabian: hier vorher mal "pl.eff_auf"  und  "pl.eff_d0"
  toadd_pleu = getData4pleu(FRM_BEF, FRM_VIS)

  # zusammenbauen DAT
  DAT = merge(toadd_agesex, toadd_tum.herz.cer.ren.liv.nurs, by= "patstuid",
              all = T,sort = F)
  DAT = merge(DAT, toadd_verwirrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_hfrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_afrq.max, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_sysbp.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_temp, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_art.ph, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_bun, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_snat, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_gluk, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_haemkrt, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_apo2.min, by= "patstuid", all = T,sort = F)
  DAT = merge(DAT, toadd_pleu, by= "patstuid", all = T,sort = F)

  # stopifnot(nrow(DAT[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(DAT[, patstuid]) == 0)
  setDF(DAT)
  # rownames(DAT) = as.character(DAT$patstuid)
  # print(hh(DAT))


  # ab hier von Fabian
  #
  #
  variables_not_timedep = c( 'age'          ,
                             'gender'       ,
                             'tumor'        ,
                             'herz'          ,
                             'cerebro'      ,
                             'renal'        ,
                             'liver'        ,
                             'nurse.home')
  DAT$gender        <- DAT$sex.0_is_male
  for(i in variables_not_timedep) {
    # print(i)
    assign(i, DAT[,i])
  }
  # print(head(age))

  # zp_fabian dependent
  variables<- c("verwirrt","hfrq.max","afrq.max","sysbp.min","temp.min",
                "temp.max","art.ph.min","bun","snat","gluk","haemkrt",
                "apo2.min","pleu_erg")
  N<-dim(DAT)[1]
  if (zp_fabian %in% event2zeitpunkt_df[EVENTKombination==F,
                                        na.omit(zp_fabianref)]){
    # war bei fabian nur c("auf","d0","d1","d2","d3","d4")
    for (i in 1:length(variables)){
      col_in_DAT <- paste(variables[i],zp_fabian,sep="_")
      if (col_in_DAT %in% colnames(DAT)){
        assign(variables[i], DAT[,col_in_DAT] )
      } else {
        assign(variables[i], rep(NA,N) )
      }
    }
  } else {
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
        dum<-worst.value(dum1,dum2,variables[i])
      }
      assign(variables[i], dum )
    }
  }

  nas <-  20 - c(is.na(age) +
                   is.na(gender) +
                   is.na(tumor) +
                   is.na(herz) +
                   is.na(cerebro) +
                   is.na(renal) +
                   is.na(liver) +
                   is.na(nurse.home) +
                   is.na(verwirrt) +
                   is.na(hfrq.max) +
                   is.na(afrq.max) +
                   is.na(sysbp.min) +
                   is.na(temp.max) +
                   is.na(art.ph.min) +
                   is.na(bun) +
                   is.na(snat) +
                   is.na(gluk) +
                   is.na(haemkrt) +
                   is.na(apo2.min) +
                   is.na(pleu_erg) )

  # print(head(age))
  # for(i in c(variables_not_timedep, variables)) {
  #   temp = get(i)
  #   names(temp) = DAT$patstuid
  #   assign(i, temp)
  # }

  # patstuid = DAT$patstuid
  # print(head(age))
  out<-psi.I.to.V(age,verwirrt,hfrq.max,afrq.max,sysbp.min,temp.min,temp.max,
                  tumor,herz,cerebro,renal,liver,
                  gender,nurse.home,art.ph.min,bun,snat,gluk,haemkrt,
                  apo2.min,pleu_erg)[,c(2,1)]
  # print(head(age))
  out<-data.frame(out,vollstaendig.von.20=nas)
  out = data.table(out)
  out$PATSTUID  = DAT$patstuid
  out$EVENT = zeitpunkt2event(zp_fabian)
  # 2020-02-25 MRos: replace call to moveColFront for no dependency on toolboxH
  # out = moveColFront(out,c( "PATSTUID", 'event'))
  out <- data.table::setcolorder(out, neworder = c( "PATSTUID", "EVENT"))
  erg = c()
  erg$input  = DAT
  erg$input2 = list(DAT$patstuid, age,verwirrt,hfrq.max,afrq.max,sysbp.min,
                    temp.min,temp.max,tumor,herz,cerebro,renal,liver,
                    gender,nurse.home,art.ph.min,bun,snat,gluk,haemkrt,
                    apo2.min,pleu_erg)
  names(erg$input2) =
    c('patstuid','age','verwirrt','hfrq.max','afrq.max','sysbp.min','temp.min',
      'temp.max','tumor','herz','cerebro','renal','liver',
      'gender','nurse.home','art.ph.min','bun','snat','gluk','haemkrt',
      'apo2.min','pleu_erg')

  erg$out  = out
  erg
}


#' Determine, if the patient has PSI class I
#'
#' @param age a variable created inside psi.fct
#' @param verwirrt a variable created inside psi.fct
#' @param hfrq.max a variable created inside psi.fct
#' @param afrq.max a variable created inside psi.fct
#' @param sysbp.min a variable created inside psi.fct
#' @param temp.min a variable created inside psi.fct
#' @param temp.max a variable created inside psi.fct
#' @param tumor a variable created inside psi.fct
#' @param herz a variable created inside psi.fct
#' @param cerebro a variable created inside psi.fct
#' @param renal a variable created inside psi.fct
#' @param liver a variable created inside psi.fct
#' @return logical vector
#' @noRd
psi.I<- function(age,verwirrt,hfrq.max,afrq.max,sysbp.min,temp.min,temp.max,
                 tumor,herz,cerebro,renal,liver){
  age[is.na(age)]                 <- 60
  verwirrt[is.na(verwirrt)]       <- 0
  hfrq.max[is.na(hfrq.max)]       <- 60
  afrq.max[is.na(afrq.max)]       <- 20
  sysbp.min[is.na(sysbp.min)]     <- 120
  temp.min[is.na(temp.min)]       <- 37
  temp.max[is.na(temp.max)]       <- 37
  tumor[is.na(tumor)]             <- 0
  herz[is.na(herz)]               <- 0
  cerebro[is.na(cerebro)]         <- 0
  renal[is.na(renal)]             <- 0
  liver[is.na(liver)]             <- 0
  class.I<- !((age>50)
              | verwirrt
              | (hfrq.max >= 125)
              | (afrq.max > 30)
              | (sysbp.min < 90)
              | (temp.min < 35)
              | (temp.max >= 40)
              | tumor
              | herz
              | cerebro
              | renal
              | liver
  )
}

#' Determine the PSI class
#'
#' @param age a variable created inside psi.fct
#' @param verwirrt a variable created inside psi.fct
#' @param hfrq.max a variable created inside psi.fct
#' @param afrq.max a variable created inside psi.fct
#' @param sysbp.min a variable created inside psi.fct
#' @param temp.min a variable created inside psi.fct
#' @param temp.max a variable created inside psi.fct
#' @param tumor a variable created inside psi.fct
#' @param herz a variable created inside psi.fct
#' @param cerebro a variable created inside psi.fct
#' @param renal a variable created inside psi.fct
#' @param liver a variable created inside psi.fct
#' @param gender a variable created inside psi.fct
#' @param nurse.home a variable created inside psi.fct
#' @param art.ph.min a variable created inside psi.fct
#' @param bun a variable created inside psi.fct
#' @param snat a variable created inside psi.fct
#' @param gluk a variable created inside psi.fct
#' @param haemkrt a variable created inside psi.fct
#' @param apo2.min a variable created inside psi.fct
#' @param pleu_erg a variable created inside psi.fct
#' @return logical vector
#' @noRd
psi.I.to.V <-function(age,verwirrt,hfrq.max,afrq.max,sysbp.min,temp.min,temp.max,tumor,herz,cerebro,renal,liver,
                      gender,nurse.home,art.ph.min,bun,snat,gluk,haemkrt,
                      apo2.min,pleu_erg){

  filt<-!(is.na(age) |
            is.na(verwirrt) |
            is.na(hfrq.max) |
            is.na(afrq.max) |
            is.na(sysbp.min) |
            is.na(temp.min) |
            is.na(temp.max) |
            is.na(tumor) |
            is.na(herz) |
            is.na(cerebro) |
            is.na(renal) |
            is.na(liver) |
            is.na(gender) |
            is.na(nurse.home) |
            is.na(art.ph.min) |
            is.na(bun) |
            is.na(snat) |
            is.na(gluk) |
            is.na(haemkrt) |
            is.na(apo2.min) |
            is.na(pleu_erg) )

  #filt<-data.frame(is.na(age) ,
  #          is.na(verwirrt) ,
  #          is.na(hfrq.max) ,
  #          is.na(afrq.max) ,
  #          is.na(sysbp.min) ,
  #          is.na(temp.min) ,
  #          is.na(temp.max) ,
  #          is.na(tumor) ,
  #          is.na(herz) ,
  #          is.na(cerebro) ,
  #          is.na(renal) ,
  #          is.na(liver) ,
  #          is.na(gender) ,
  #          is.na(nurse.home) ,
  #          is.na(art.ph.min) ,
  #          is.na(bun) ,
  #          is.na(snat) ,
  #          is.na(gluk) ,
  #          is.na(haemkrt) ,
  #          is.na(apo2.min) ,
  #          is.na(pleu_erg)  )

  # pruefen ob Patient in Klasse 1 ist
  psi.value.I<-psi.I(age,verwirrt,hfrq.max,afrq.max,sysbp.min,temp.min,temp.max,tumor,herz,cerebro,renal,liver)
  N<-length(age)
  psi.class<-c()

  age[is.na(age)]                     <- 60  # bei unbekannt nehmen wir Alter 60 an (entspricht etwa Mittelwert)
  gender[is.na(gender)]               <- 0   # bei unbekannt nehmen wir maennlich an
  verwirrt[is.na(verwirrt)]           <- 0   #
  hfrq.max[is.na(hfrq.max)]           <- 60  #
  afrq.max[is.na(afrq.max)]           <- 20  #
  sysbp.min[is.na(sysbp.min)]         <- 120 #
  temp.min[is.na(temp.min)]           <- 37  #       bei unbekannten Werten nehmen wir Werte an, die nicht
  temp.max[is.na(temp.max)]           <- 37  #       zur Erhoehung des Index fuehren
  tumor[is.na(tumor)]                 <- 0   #       ausser bei Alter und Geschlecht - dort geht das nicht
  herz[is.na(herz)]                   <- 0   #
  cerebro[is.na(cerebro)]             <- 0   #
  renal[is.na(renal)]                 <- 0   #
  liver[is.na(liver)]                 <- 0   #
  nurse.home[is.na(nurse.home)]       <- 0   #
  art.ph.min[is.na(art.ph.min)]       <- 7.5 #
  bun[is.na(bun)]                     <- 7.5 #
  snat[is.na(snat)]                   <- 150 #
  gluk[is.na(gluk)]                   <- 10  #
  haemkrt[is.na(haemkrt)]             <- 0.4 #
  apo2.min[is.na(apo2.min)]           <- 70 #
  pleu_erg[is.na(pleu_erg)]           <- 0   #


  for (i in 1:N){
    age.i         <- age[i]
    verwirrt.i    <- verwirrt[i]
    hfrq.max.i    <- hfrq.max[i]
    afrq.max.i    <- afrq.max[i]
    sysbp.min.i   <- sysbp.min[i]
    temp.min.i    <- temp.min[i]
    temp.max.i    <- temp.max[i]
    tumor.i       <- tumor[i]
    herz.i        <- herz[i]
    cerebro.i     <- cerebro[i]
    renal.i       <- renal[i]
    liver.i       <- liver[i]
    gender.i      <- gender[i]
    nurse.home.i  <- nurse.home[i]
    art.ph.min.i  <- art.ph.min[i]
    bun.i         <- bun[i]
    snat.i        <- snat[i]
    gluk.i        <- gluk[i]
    haemkrt.i     <- haemkrt[i]
    apo2.min.i    <- apo2.min[i]
    pleu_erg.i    <- pleu_erg[i]
    psi.value.I.i <- psi.value.I[i]

    psi.class[i]<-1
    if (!psi.value.I.i){
      # count zaehlt die Punkte um zw. Klassen 2 bis 5 zu unterscheiden
      count<-0
      if (gender.i == 0){
        count<-count + age.i
      } else {
        count<-count + age.i - 10
      }
      if (nurse.home.i) {
        count<-count + 10
      }
      if (tumor.i) {
        count<-count + 30
      }
      if (liver.i) {
        count<-count + 20
      }
      if (herz.i) {
        count<-count + 10
      }
      if (cerebro.i) {
        count<-count + 10
      }
      if (renal.i) {
        count<-count + 10
      }
      if (verwirrt.i) {
        count<-count + 20
      }
      if (hfrq.max.i >= 125) {
        count<-count + 10
      }
      if (afrq.max.i > 30) {
        count<-count + 20
      }
      if (sysbp.min.i < 90) {
        count<-count + 20
      }
      if ((temp.min.i < 35)|(temp.max.i >=40)) {
        count<-count + 15
      }
      if (art.ph.min.i < 7.35) {
        count<-count + 30
      }
      if (bun.i >= 9) {
        count<-count + 20
      }
      if (snat.i < 130) {
        count<-count + 20
      }
      if (gluk.i >= 14) {
        count<-count + 10
      }
      if (haemkrt.i < 0.3) {
        count<-count + 10
      }
      if (apo2.min.i < 60) {
        count<-count + 10
      }
      if (pleu_erg.i) {
        count<-count + 10
      }
      # Klasse in Abhaengigkeit von count zuordnen
      if (count <= 70){
        psi.class[i]<- 2
      }
      if ((count > 70) & (count <=90)){
        psi.class[i]<- 3
      }
      if ((count > 90) & (count <=130)){
        psi.class[i]<- 4
      }
      if (count > 130){
        psi.class[i]<- 5
      }
    }

  }

  # sum(filt)
  # if(sum(filt)>0){
  #   par(mfrow=c(1,2))
  #   barplot(table(psi.class[filt]),main="PSI Werte",sub="Personen mit Missings entfernt")
  #   barplot(table(psi.class),main="PSI Werte",sub="Missings durch unkrit. Werte aufgefuellt")
  # } else {
  #   par(mfrow=c(1,1))
  #   #barplot(table(psi.class[filt]),main="PSI Werte",sub="Personen mit Missings entfernt")
  #   barplot(table(psi.class),main="PSI Werte",sub="Missings durch unkrit. Werte aufgefuellt")
  # }

  psi.class.filt<-psi.class
  psi.class.filt[!filt]<-NA

  return(cbind(psi.class.filt,psi.class))
}
