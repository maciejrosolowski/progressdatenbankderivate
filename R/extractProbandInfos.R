#' get patient characteristics from the table DID_PROBAND
#'
#' @param DID_PROBAND data.table containing the table DID_PROBAND from the
#' database of the PROGRESS study
#' @param parameter a character vector with the names of the charcteristics
#'  to be extracted.
#'
#' @return a data.table with the PATSTUID and the extracted or computed patient
#' characteristics.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_PROBAND <- readxl::read_excel(excel_fn, "DID_PROBAND")
#' data.table::setDT(DID_PROBAND)
#' extractProbandInfos(DID_PROBAND, parameter = "BMI")
#' extractProbandInfos(DID_PROBAND, parameter = c("BMI", "AGE"))
#' }
extractProbandInfos = function(DID_PROBAND,
                               parameter =  c('ZENTRUM', 'AGE', 'SEX',
                                              'WEIGHT', 'HEIGHT', "BMI", 'COB',
                                              'COBM', 'COBF',  'ABKDAYS_BEVOR',
                                              'CAPSYS')) {
  parameter <- match.arg(parameter, several.ok = TRUE)
  # due to non-standard evaluation notes in R CMD check
  BMI <- HEIGHT <- SEX <- WEIGHT <- NULL
  # parameter =  c('ZENTRUM', 'AGE', 'SEX', 'WEIGHT', 'HEIGHT', "BMI", 'COB',
  # 'COBM', 'COBF',  'ABKDAYS_BEVOR', 'CAPSYS')
  if ("SEX" %in% parameter) {
    DID_PROBAND[,SEX := removeMissingCodes(SEX)]
  }
  if ("BMI" %in% parameter) {
    DID_PROBAND[,BMI:= WEIGHT/(HEIGHT/100)^2]
  }
  if (any(c("COB", "COBM", "COBF") %in% parameter)) {
    message("'COB', 'COBM', 'COBF' means country of birth proband/mother/father")
  }
  DID_PROBAND[,c('PATSTUID', parameter), with = F]
}
