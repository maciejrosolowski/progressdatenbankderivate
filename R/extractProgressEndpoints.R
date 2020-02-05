#' get age and sex from the table DID_PROBAND
#'
#' @param DID_SOFA data.table containing the table DID_SOFA from the database
#' of the PROGRESS study
#' @param DID_EP_SERDIS as above
#' @param DID_CLIN as above
#' @param FRM_DIL_LABORWERTE as above
#' @param critMindVerschlechterung I do not know
#' @param sofaEPonly I do not know
#'
#' @return amed list with two elements "detailed" and "compact" containing
#' data.tables.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_SOFA <- readxl::read_excel(excel_fn, "DID_SOFA")
#' DID_EP_SERDIS <- readxl::read_excel(excel_fn, "DID_EP_SERDIS")
#' DID_CLIN <- readxl::read_excel(excel_fn, "DID_CLIN")
#' FRM_DIL_LABORWERTE <- readxl::read_excel(excel_fn, "FRM_DIL_LABORWERTE")
#' data.table::setDT(DID_SOFA)
#' data.table::setDT(DID_EP_SERDIS)
#' data.table::setDT(DID_CLIN)
#' data.table::setDT(FRM_DIL_LABORWERTE)
#' extractProgressEndpoints(DID_SOFA, DID_EP_SERDIS, DID_CLIN,
#' FRM_DIL_LABORWERTE)
#' }
extractProgressEndpoints = function(DID_SOFA, DID_EP_SERDIS = DID_EP_SERDIS,
                                    DID_CLIN = DID_CLIN,
                                    FRM_DIL_LABORWERTE = FRM_DIL_LABORWERTE,
                                    critMindVerschlechterung = 4, sofaEPonly = F)
{
  # due to non-standard evaluation notes in R CMD check
  futureEP_pre <- EP <- futureEP_1st <- futureEP <- BEATMUNG <-
    future_ventilation_pre <- future_ventilation_1st <- future_ventilation <-
    future_dead <- TOD <- future_dead_1st <- PAT_EVENT <- worstfutureSOFA <-
    SUPPL_SOFA <- sofa_maxVerschlechtung <- SOFA_Tp0 <- worstfutureSOFATp1st <-
    SUPPL_npSOFA <- SUPPL_PULMONAL <- PAT_EVENT2 <- oxi.ind <- CLIN_PARAM <-
    WERT <- billiLOG <- kreaLOG <- map <- typ <- variable <- tp <- patstuid <-
    thrombo_min <- value <- thrombo_dbb <- gcs <- newvar <- variab <- future_days <-
    num <- zeitpunktref_future <- id_gx <- id_future <- zeitpunktref <-
    EVENT <- PATSTUID <- NULL


  DID_EP_SERDIS = copy(DID_EP_SERDIS)
  DID_SOFA = copy(DID_SOFA)
  # DID_EP_SERDIS$futureEP = NULL
  # DID_EP_SERDIS$futureEP_pre = NULL
  DID_EP_SERDIS[,futureEP_pre:= as.numeric(all(EP[EVENT<=3] ==0) &
                                             (any(EP[EVENT>3]==1)) , 1, 0), by = PATSTUID]

  # DID_EP_SERDIS[ futureEP_pre ==1 & EP==0& EVENT==1,.(PATSTUID, EP, EVENT,futureEP_pre)]
  stopifnot(nrow(DID_EP_SERDIS[ futureEP_pre ==1 & EP==1& EVENT==1,.(PATSTUID, EP, EVENT,futureEP_pre)])==0)
  # DID_EP_SERDIS[,.N,futureEP_pre]
  DID_EP_SERDIS[, futureEP_1st := min(EVENT[EP==1], na.rm = T), by = PATSTUID] # war bis v010 futureEP_pre==1 filtered
  DID_EP_SERDIS[is.infinite(futureEP_1st)==T , futureEP_1st:= NA]
  # DID_EP_SERDIS[,.N,futureEP_1st]

  DID_EP_SERDIS[futureEP_pre==1, futureEP := ifelse((EVENT < futureEP_1st) & is.na(EP)==F, 1,  NA), by = PATSTUID]

  DID_EP_SERDIS[, futureEP := ifelse(is.na(futureEP) &
                                       all(na.omit(EP)==0) &
                                       sum(EP[EVENT>=3]==0,na.rm=T)>=1 &
                                       is.na(EP)==F, 0, futureEP) %>%  as.numeric, by = PATSTUID]
  # DID_EP_SERDIS[,.N, futureEP]

  # DID_EP_SERDIS[futureEP_pre==1, .(PATSTUID, futureEP_pre, futureEP, futureEP_1st,EVENT,EP)][PATSTUID %in% unique(PATSTUID)[11:13]][order(PATSTUID,EVENT)] %>% unique %>%  data.frame

  # DID_EP_SERDIS[PATSTUID==10068,.(PATSTUID, futureEP_pre, futureEP, futureEP_1st,EVENT,EP)] [order(PATSTUID,EVENT)] %>% unique %>%  data.frame


  # # future ventilation  ####
  DID_EP_SERDIS[,.N, BEATMUNG]
  # DID_EP_SERDIS$future_ventilation = NULL
  # DID_EP_SERDIS$future_ventilation_pre = NULL
  DID_EP_SERDIS[,future_ventilation_pre:= as.numeric(all(BEATMUNG[EVENT<=3] ==0 ) &
                                                       (any(BEATMUNG[EVENT>3]==1)) , 1, 0), by = PATSTUID]

  DID_EP_SERDIS[ future_ventilation_pre ==1 & BEATMUNG==0& EVENT==1,.(PATSTUID, BEATMUNG, EVENT,future_ventilation_pre)]
  stopifnot(nrow(DID_EP_SERDIS[ future_ventilation_pre ==1 &
                                  BEATMUNG==1 &
                                  EVENT==1,
                                .(PATSTUID, BEATMUNG, EVENT,future_ventilation_pre)])==0)


  DID_EP_SERDIS[future_ventilation_pre==1,
                future_ventilation_1st := min(EVENT[BEATMUNG==1], na.rm = T), by = PATSTUID]

  DID_EP_SERDIS[future_ventilation_pre==1,
                future_ventilation := ifelse((EVENT < future_ventilation_1st) &
                                               is.na(BEATMUNG)==F, 1,  NA), by = PATSTUID]

  # wenn ich kein fall bin und ueberall sie beatmung 0 oder NA ist und
  # mindestens einmal ab einschluss eine beatmung nachgewiesen wurde und
  # NA beim EVENT selber nicht NA ist, dann bin ich kontrolle
  DID_EP_SERDIS[, future_ventilation := ifelse(is.na(future_ventilation) &
                                                 all(na.omit(BEATMUNG)==0) &
                                                 sum(BEATMUNG[EVENT>=3]==0,na.rm=T)>=1 &
                                                 is.na(BEATMUNG)==F, 0, future_ventilation) %>%
                  as.numeric, by = PATSTUID]



  # DID_EP_SERDIS[future_ventilation_pre==1, .(PATSTUID, future_ventilation_pre, future_ventilation, future_ventilation_1st,EVENT,BEATMUNG)][PATSTUID %in% unique(PATSTUID)[11:13]][order(PATSTUID,EVENT)] %>% unique %>%  data.frame

  # # future dead  ####
  # DID_EP_SERDIS[,.N, TOD]
  # DID_EP_SERDIS[,future_dead:= NULL]
  DID_EP_SERDIS[,future_dead:= as.numeric(
    ifelse(any(na.omit(TOD)==1) , 1, ifelse(all(TOD[EVENT!=1]==0),  0, NA))), by = PATSTUID]
  DID_EP_SERDIS[future_dead==1, future_dead_1st := min(EVENT[TOD==1], na.rm = T), by = PATSTUID]
  # DID_EP_SERDIS[PATSTUID==10120,       .(PATSTUID, future_dead, future_dead_1st,EVENT,TOD)]

  DID_EP_SERDIS[ future_dead ==1 & TOD==0& EVENT==1,.(PATSTUID, TOD, EVENT,future_dead)]
  stopifnot(nrow(DID_EP_SERDIS[ future_dead ==1 & TOD==1& EVENT==1,.(PATSTUID, TOD, EVENT,future_dead)])==0)


  # DID_EP_SERDIS[future_dead ==1, .(PATSTUID, future_dead, future_dead_1st,EVENT,TOD)][PATSTUID %in% unique(PATSTUID)[11:16]][order(PATSTUID,EVENT)] %>% unique %>%  data.frame

  # DID_EP_SERDIS[is.na(TOD) & future_dead ==0, .(PATSTUID, future_dead, future_dead_1st,EVENT,TOD)][PATSTUID %in% unique(PATSTUID)[11:16]][order(PATSTUID,EVENT)] %>% unique %>%  data.frame

  # DID_EP_SERDIS[ PATSTUID %in% c(1889,23075), .(PATSTUID, future_dead, future_dead_1st,EVENT,TOD)][order(PATSTUID,EVENT)]%>% unique %>%  data.frame

  # DID_EP_SERDIS[,.N, .(TOD, future_dead, EVENT==1)]

  # # worst future sofa
  # NOTE das ist die zeitpunkt 0 variante. besser gepauert waere die bygl. erstem gx messung yuguenftige, da nicht alle bei 0 anfangen
  # qlist1 = venn2(DID_EP_SERDIS$PAT_EVENT, DID_SOFA$PAT_EVENT)
  stopifnot(all(DID_SOFA$PATSTUID %in% DID_EP_SERDIS$PATSTUID))

  DID_EP_SERDIS[,PAT_EVENT := paste0(PATSTUID, "_", EVENT)]
  DID_SOFA[,PAT_EVENT := paste0(PATSTUID, "_", EVENT)]

  DID_EP_SERDIS =
    merge(DID_EP_SERDIS, DID_SOFA[,c("PAT_EVENT",
                                     "CALC_PULMONAL", "CALC_CARDIOVASC",
                                     "CALC_COAGULATION", "CALC_KIDNEY", "CALC_LIVER", "CALC_CNS",
                                     "CALC_SOFA", "CALC_COMPLETE",
                                     "SUPPL_PULMONAL", "SUPPL_CARDIOVASC",
                                     "SUPPL_COAGULATION", "SUPPL_KIDNEY", "SUPPL_LIVER", "SUPPL_CNS",
                                     "SUPPL_SOFA", "SUPPL_COMPLETE"),
                                  with = F], by  = "PAT_EVENT", all = T, sort = F)





  DID_EP_SERDIS[,worstfutureSOFA :=
                  ifelse(all(is.na(SUPPL_SOFA[EVENT>3])),
                         NA_real_,
                         max(SUPPL_SOFA[EVENT>3], na.rm = T)),
                PATSTUID]

  # ## 	Oder SOFA-Verschlechterer----
  DID_EP_SERDIS[,.N, EVENT]
  DID_EP_SERDIS[,sofa_maxVerschlechtung:=
                  ifelse(all(is.na(SUPPL_SOFA[EVENT>3])),
                         NA_real_,
                         max(SUPPL_SOFA[EVENT>3], na.rm = T) - SUPPL_SOFA[EVENT==3]
                  ),
                by = PATSTUID]
  DID_EP_SERDIS[,.N, sofa_maxVerschlechtung] %>% data.frame()

  DID_EP_SERDIS[, SOFA_Tp0 := SUPPL_SOFA[EVENT==3], PATSTUID]

  DID_EP_SERDIS[, worstfutureSOFATp1st :=
                  ifelse(all(is.na(SUPPL_SOFA[EVENT>3])),
                         NA_real_,
                         min(EVENT[EVENT>3 & SUPPL_SOFA==worstfutureSOFA], na.rm = T)),
                by = PATSTUID]

  DID_EP_SERDIS[, .N,worstfutureSOFATp1st]
  DID_EP_SERDIS[is.na(worstfutureSOFATp1st), .N, sofa_maxVerschlechtung]

  DID_EP_SERDIS[,(paste0('sofa_verschlechtererMin',critMindVerschlechterung)) :=
                  ifelse(sofa_maxVerschlechtung < critMindVerschlechterung |
                           is.na(sofa_maxVerschlechtung),
                         0, 1) ]

  DID_EP_SERDIS[, (paste0('sofa_verschlechtererMin',critMindVerschlechterung, "Tp1st")) :=
                  ifelse(all(is.na(SUPPL_SOFA[EVENT>3 & SUPPL_SOFA>=SOFA_Tp0+critMindVerschlechterung])),
                         NA_real_,
                         min(EVENT[EVENT>3 & SUPPL_SOFA>= SOFA_Tp0+critMindVerschlechterung], na.rm = T)),
                by = PATSTUID]

  DID_EP_SERDIS[, SUPPL_npSOFA := SUPPL_SOFA-SUPPL_PULMONAL]

  setorder(DID_EP_SERDIS, PATSTUID, EVENT)
  # DID_EP_SERDIS[get(paste0('sofa_verschlechtererMin',critMindVerschlechterung))==T][PATSTUID %in% unique(PATSTUID)[11]]
  # DID_EP_SERDIS[duplicated(PATSTUID)==F, .N, worstfutureSOFA]
  #
  if(sofaEPonly==T) {
    resi = c()
    resi$detail = DID_EP_SERDIS

    resi$compact =
      DID_EP_SERDIS[,c("PAT_EVENT", "PATSTUID", "EVENT", "EP", "TOD", "ITS", "BEATMUNG",
                       "O2GE6LMIN", "KATECHO", "DIALYSE", "futureEP_1st", "futureEP",
                       "future_ventilation_1st", "future_ventilation", "future_dead",
                       "future_dead_1st", "CALC_PULMONAL", "CALC_CARDIOVASC", "CALC_COAGULATION",
                       "CALC_KIDNEY", "CALC_LIVER", "CALC_CNS", "CALC_SOFA", "CALC_COMPLETE",
                       "SUPPL_PULMONAL", "SUPPL_CARDIOVASC", "SUPPL_COAGULATION", "SUPPL_KIDNEY",
                       "SUPPL_LIVER", "SUPPL_CNS", "SUPPL_SOFA", "SUPPL_COMPLETE", "worstfutureSOFA",
                       "sofa_maxVerschlechtung", "SOFA_Tp0", "worstfutureSOFATp1st",
                       "SUPPL_npSOFA", "sofa_verschlechtererMin4", "sofa_verschlechtererMin4Tp1st"
    ), with = F]

    return(resi)
  }

  # subsofa kram


  DID_CLIN[, PAT_EVENT2 := paste0(PATSTUID, "_", EVENT)]
  stopifnot(anyDuplicated(DID_CLIN, by = c("CLIN_PARAM", "PAT_EVENT2")) == 0)
  DID_EP_SERDIS[
    , oxi.ind := DID_CLIN[CLIN_PARAM=="OxygenIndex-MIN"][
      # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT)]
      match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT)]
    ]
  DID_EP_SERDIS[
    , billiLOG := DID_CLIN[CLIN_PARAM=="GBILI-MAX"][
      # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT) %>% log]
      match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT) %>% log]
    ]
  DID_EP_SERDIS[
    , kreaLOG := DID_CLIN[CLIN_PARAM=="SKREA-MAX"][
      # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT) %>% log]
      match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT) %>% log]
    ]
  DID_EP_SERDIS[, map := DID_CLIN[CLIN_PARAM=="MAP-MIN"][
    # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT)]
    match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT)]
    ]

  thrombodat =  getData4thrombo(FRM_DIL_LABORWERTE,DID_CLIN )
  thrombodat2 = melt(thrombodat, id.vars = "patstuid")
  thrombodat2[,typ := stringr::str_split(variable, "_") %>% sapply(., "[",2)]
  thrombodat2[,tp := stringr::str_split(variable, "_") %>% sapply(., "[",3)]
  thrombodat2[,EVENT:= zeitpunkt2event(zp_fabian = tp)]
  thrombodat2[, PAT_EVENT2 := paste0(patstuid, "_", EVENT)]

  stopifnot(anyDuplicated(DID_EP_SERDIS$PAT_EVENT2) == 0)

  DID_EP_SERDIS[, thrombo_min   :=   thrombodat2[typ=="min"][
    # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(value)]
    match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(value)]
    ]
  DID_EP_SERDIS[, thrombo_dbb   :=   thrombodat2[typ=="dbb"][
    # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(value)]
    match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(value)]
    ]

  # DID_EP_SERDIS[,plot(thrombo_min~thrombo_dbb)]

  # DID_EP_SERDIS[,boxplot(thrombo_min~SUPPL_COAGULATION, main = "thrombo_min~SUPPL_COAGULATION")$x]
  # mtext("thrombo_min um den predizierten sofa score aufzudroeseln, weil der dem Sofa entspricht")

  # DID_EP_SERDIS[,boxplot(thrombo_dbb~SUPPL_COAGULATION, main = "thrombo_dbb~SUPPL_COAGULATION")$x]
  # mtext("thrombo_dbb fuer querschnittlich, weil die entsprechnde Blutprobe der gleiche Zeitpunkt wie die Gx ist.")

  DID_EP_SERDIS[, gcs  :=   DID_CLIN[CLIN_PARAM=="GCS-SUM"][
    # match_hk(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT)]
    match(DID_EP_SERDIS$PAT_EVENT, PAT_EVENT2), as.numeric(WERT)]
    ]

  ## get future phenos
  DID_EP_SERDIS[is.na(SUPPL_SOFA)==F, .N, EVENT]

  todo = data.table(expand.grid(future_days = 1:4,
                                variab = c("SUPPL_SOFA", "SUPPL_npSOFA",
                                           'oxi.ind', 'billiLOG' , 'kreaLOG'  ,
                                           'map', 'thrombo_min',  'gcs'),
                                stringsAsFactors = F))

  todo[, newvar := paste0(variab,"__plus", future_days, "day")]
  todo[,num := 1:.N]

  addFutureVariable <- function(future_days, variab, DID_EP_SERDIS=DID_EP_SERDIS) {
    # future_days = myzeile$future_days; variab = myzeile$variab
    DID_EP_SERDIS[,zeitpunktref := event2zeitpunkt(EVENT = EVENT, returnformat = "zeitpunktref")]
    DID_EP_SERDIS[,zeitpunktref_future := zeitpunktref + future_days]
    DID_EP_SERDIS[,id_gx := paste(PATSTUID, zeitpunktref)]
    DID_EP_SERDIS[,id_future := paste(PATSTUID, zeitpunktref_future)]
    variab_future = paste0(variab,"__plus", future_days, "day")
    # DID_EP_SERDIS[,(variab_future) := DID_EP_SERDIS[match_hk(id_future,id_gx), get(variab)]]
    stopifnot(anyDuplicated(DID_EP_SERDIS$variab) == 0)
    DID_EP_SERDIS[,(variab_future) := DID_EP_SERDIS[match(id_future,id_gx), get(variab)]]
    DID_EP_SERDIS$zeitpunktref = NULL
    DID_EP_SERDIS$zeitpunktref_future = NULL
    DID_EP_SERDIS$id_gx = NULL
    DID_EP_SERDIS$id_future = NULL

    DID_EP_SERDIS
  }

  for(mynum in todo$num) {
    # mynum=1

    myzeile = todo[num == mynum]
    # message("working on ", myzeile$newvar)
    DID_EP_SERDIS = addFutureVariable(future_days = myzeile$future_days,
                                      variab = myzeile$variab, DID_EP_SERDIS)
  }

  # DID_EP_SERDIS[PATSTUID==unique(PATSTUID)[8], grep("PATSTUID|EVENT|SUPPL_SOFA|billi", names(DID_EP_SERDIS)), with= F]

  resi = c()
  resi$detail = DID_EP_SERDIS

  resi$compact =
    DID_EP_SERDIS[,c("PAT_EVENT","PATSTUID", "EVENT", "EP", "TOD", "ITS", "BEATMUNG",
                     "O2GE6LMIN", "KATECHO", "DIALYSE",
                     "futureEP","futureEP_1st",
                     "future_ventilation",  "future_ventilation_1st",
                     "future_dead", "future_dead_1st",
                     "CALC_PULMONAL", "CALC_CARDIOVASC", "CALC_COAGULATION", "CALC_KIDNEY", "CALC_LIVER",
                     "CALC_CNS", "CALC_SOFA", "CALC_COMPLETE",
                     "SUPPL_PULMONAL",'SUPPL_npSOFA', "SUPPL_CARDIOVASC",
                     "SUPPL_COAGULATION", "SUPPL_KIDNEY", "SUPPL_LIVER", "SUPPL_CNS",
                     "SUPPL_SOFA", "SUPPL_COMPLETE",
                     "worstfutureSOFA","worstfutureSOFATp1st", "sofa_maxVerschlechtung",
                     "SOFA_Tp0",
                     paste0('sofa_verschlechtererMin',critMindVerschlechterung),
                     paste0('sofa_verschlechtererMin',critMindVerschlechterung, "Tp1st"),
                     c("oxi.ind", "billiLOG", "kreaLOG", "map", "thrombo_min", "gcs"), todo$newvar) %>%
                    unique, with = F]
  resi
}

# utils::globalVariables(
#   c("futureEP_pre", "EP", "futureEP_1st", "futureEP", "BEATMUNG",
#     "future_ventilation_pre", "future_ventilation_1st", "future_ventilation",
#     "future_dead", "TOD", "future_dead_1st", "PAT_EVENT", "worstfutureSOFA",
#     "SUPPL_SOFA", "sofa_maxVerschlechtung", "SOFA_Tp0", "worstfutureSOFATp1st",
#     "SUPPL_npSOFA", "SUPPL_PULMONAL", "PAT_EVENT2", "oxi.ind", "CLIN_PARAM",
#     "WERT", "billiLOG", "kreaLOG", "map", "typ", "variable", "tp", "patstuid",
#     "thrombo_min", "value", "thrombo_dbb", "gcs", "newvar", "variab",
#     "future_days", "num", "zeitpunktref_future", "id_gx", "id_future",
#     "zeitpunktref"))
