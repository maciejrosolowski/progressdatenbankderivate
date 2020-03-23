#' compute the comorbidity score comorbscore
#'
#' @param FRM_BAS data.table containing the table FRM_BAS from the database
#' of the PROGRESS study
#'
#' @return named list with two elements "detailed" and "compact" containing
#' data.tables with the computed comorbscore and its components.
#'
#' @details The comorbscore is the modified Charlson score
#' (Charlson, Pompei, Ales, and MacKenzie, 1987) and is computed as a sum of the
#' following points:
#' \itemize{
#' \item{} HI (heart failure, Herzinsuffizienz) or SCHRHERZ
#' (other chronic heart problem): 1 point,
#' \item{} CEREBROERK (chronic cerebr. disease, Chronische Cerebr. Erkrankung):
#'  1 point
#' \item{} CHRNIERE (chronic renal failure, chronische Nierenerkrankung):
#' 2 points
#' \item{} CHRLEBER (chronic liver disease, chronische Lebererkrankung):
#' 3 points
#' \item{} DIABETES: 1 point
#' \item{} CHRATEMLUNG (chronic lung disease,
#' Chronische Atemwegs/Lungenerkrankung): 1 point
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BAS <- readxl::read_excel(excel_fn, "FRM_BAS")
#' data.table::setDT(FRM_BAS)
#' extractComorbScore(FRM_BAS)
#' }
extractComorbScore = function(FRM_BAS) {
  # due to non-standard evaluation notes in R CMD check
  SCHRHERZorHI <- SCHRHERZ <- HI <- CEREBROERK <- CHRNIERE <- CHRLEBER <-
    DIABETES <- CHRATEMLUNG <- SCHRHERZorHIweighted <- CEREBROERKweighted <-
    CHRNIEREweighted <- CHRLEBERweighted <- DIABETESweigted <-
    CHRATEMLUNGweighted <- comorbscore <- PATSTUID <- EVENT <- NULL

  #### Matching details
  # - in the statistical analysis of PROGRESS GE (mixed model) I adjusted for
  # 'age', 'bmi','sex.0_is_male', 'sum_sofa', 'rauchen_anz_jahre'
  # - in the RIBOLUTION work I matched for  **sex.0_is_male+  age + bmi +
  # comorbscore  + sum_sofa via method = "optimal"**
  #   - comorbscore was defined as  **herz +cerebro + renal + liver +
  # diabetes + chr.lunge**
  #
  #   Note that in the RIBOLUTION-comorbscore, *herz* was defined as cardiac
  # insufficiency. When discussing this,  Peter and I realized, that
  # herz.other == SCHRHERZ == other chronical heart disease should be also
  # valid
  #
  # As suggested by Sebastian, we now use a modified Charlson score
  # `r citep("10.1016/0021-9681(87)90171-8")`as follows:
  #
  # - Herzinssuffizienz oder sonstige Chronische Herzerkrankungen vorhanden: 1 point
  # - Chronische Cerebr. Erkrankung: 1 point
  # - Chronische Nierenerkrankung: 2 point
  # - Chronische Lebererkrankung: 3 point
  # - Diabetes: 1 point
  # - Chronische Atemwegs/Lungenerkrankung: 1 point
  #

  FRM_BAS_copy <- copy(FRM_BAS)
  # message(" ==> Setting -1 and 99 in variables 'HI', 'SCHRHERZ', 'CEREBROERK',
  #         'CHRNIERE','CHRLEBER','DIABETES','CHRATEMLUNG' to NA")
  vars4comorb = c('HI', 'SCHRHERZ',	 'CEREBROERK' ,'CHRNIERE','CHRLEBER',
                  'DIABETES','CHRATEMLUNG')

  for(i in vars4comorb) {
    # message('looking at ' , i)
    FRM_BAS_copy[, .N, get(i)]
    FRM_BAS_copy[get(i) %in% c(-1,99), (i) := NA]
    FRM_BAS_copy[, .N, get(i)]
  }

  # message(" ==> considering SCHRHERZ or HI as Heart-Disease")
  FRM_BAS_copy[,SCHRHERZorHI := ifelse(SCHRHERZ==1 | HI ==1, 1, 0)]
  FRM_BAS_copy[,.N,.(SCHRHERZorHI, SCHRHERZ, HI)]

  ## codierung checken
  FRM_BAS_4comorbscore  = FRM_BAS_copy[, .(PATSTUID=as.character(PATSTUID),
                                           HI, SCHRHERZ, SCHRHERZorHI ,
                                           CEREBROERK ,CHRNIERE,CHRLEBER,
                                           DIABETES,CHRATEMLUNG)]
  # stopifnot(nrow(FRM_BAS_4comorbscore[allDuplicatedEntries(PATSTUID)])==0)
  stopifnot(anyDuplicated(FRM_BAS_4comorbscore[, PATSTUID]) == 0)

  # showNA(FRM_BAS_4comorbscore)

  # ### weighting comorb ----------------
  # message(" ==> Weighting variabls as follows: SCHRHERZorHIweighted * 1,
  #         CEREBROERK * 1, CHRNIERE * 2, CHRLEBER * 3, DIABETES * 1,
  #         CHRATEMLUNG * 1")
  FRM_BAS_4comorbscore[,SCHRHERZorHIweighted := SCHRHERZorHI*1]
  FRM_BAS_4comorbscore[,CEREBROERKweighted := CEREBROERK*1]
  FRM_BAS_4comorbscore[,CHRNIEREweighted := CHRNIERE*2]
  FRM_BAS_4comorbscore[,CHRLEBERweighted := CHRLEBER*3]
  FRM_BAS_4comorbscore[,DIABETESweigted := DIABETES*1]
  FRM_BAS_4comorbscore[,CHRATEMLUNGweighted := CHRATEMLUNG*1]

  # ### fix missing data comorbscore
  indsWithMissing = FRM_BAS_4comorbscore[
    , is.na(sum(.SD, na.rm = F)),
    .SDcols = c('SCHRHERZorHI','CEREBROERKweighted','CHRNIEREweighted',
                'CHRLEBERweighted','DIABETESweigted','CHRATEMLUNGweighted'),
    by = PATSTUID]

  message(" ==> Considering missing information as healthy, observed in ",
          sum(indsWithMissing$V1), ' of ' ,nrow(indsWithMissing )," individuals")

  # missing considered 0, i.e. no comorbidity, regardles how many missings are observed
  FRM_BAS_4comorbscore[, comorbscore :=  sum(.SD, na.rm = T),
                       .SDcols = c('SCHRHERZorHIweighted','CEREBROERKweighted',
                                   'CHRNIEREweighted','CHRLEBERweighted',
                                   'DIABETESweigted','CHRATEMLUNGweighted'),
                       by = PATSTUID]
  FRM_BAS_4comorbscore

  resi = c()
  FRM_BAS_4comorbscore[,PATSTUID:= as.numeric(PATSTUID)]
  resi$detailed = FRM_BAS_4comorbscore
  resi$compact = FRM_BAS_4comorbscore[,c("PATSTUID", "SCHRHERZorHIweighted",
                                         "CEREBROERKweighted",
                                         "CHRNIEREweighted",
                                         "CHRLEBERweighted",
                                         "DIABETESweigted",
                                         "CHRATEMLUNGweighted",
                                         "comorbscore"), with = F]
  resi
}

# utils::globalVariables(c("SCHRHERZorHI", "SCHRHERZ", "HI", "CEREBROERK",
#                          "CHRNIERE", "CHRLEBER", "DIABETES", "CHRATEMLUNG",
#                          "SCHRHERZorHIweighted", "CEREBROERKweighted",
#                          "CHRNIEREweighted", "CHRLEBERweighted",
#                          "DIABETESweigted", "CHRATEMLUNGweighted",
#                          "comorbscore"))
