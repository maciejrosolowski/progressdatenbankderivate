#' get age and sex from the table DID_PROBAND
#'
#' @param DID_PROBAND data.table containing the table DID_PROBAND from the database
#' of the PROGRESS study
#' @param change_names a logical value. If TRUE, then the names of the patient
#' characteristics will be changed. This is expected by the functions which
#' computed the severity scores.
#'
#' @return data.table with the ID of the patient (patstuid), age and sex
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_PROBAND <- readxl::read_excel(excel_fn, 'DID_PROBAND')
#' data.table::setDT(DID_PROBAND)
#' toadd_agesex <- getData4age.sex(DID_PROBAND)
#' toadd_agesex <- getData4age.sex(DID_PROBAND, change_names = FALSE)
#' }
getData4age.sex = function(DID_PROBAND, change_names = TRUE) {
  # due to non-standard evaluation notes in R CMD check
  SEX <- sex.0_is_male <- patstuid <- PATSTUID <- AGE <- NULL
  # nse_vars <- c("SEX", "sex.0_is_male", "patstuid", "PATSTUID", "AGE")
  # eval(parse(text = paste(paste(nse_vars, collapse = " <- "), "<- NULL")), envir = parent.frame())
  # stop if any duplicated PATSTUIDs
  # stopifnot(nrow(DID_PROBAND[toolboxH::allDuplicatedEntries(PATSTUID)])==0)
  stopifnot(anyDuplicated(DID_PROBAND[, PATSTUID]) == 0)
  toadd_agesex = DID_PROBAND[,.(patstuid = PATSTUID, AGE, sex.0_is_male = 1-SEX ) ]
  toadd_agesex[!(sex.0_is_male %in% c(0,1)), sex.0_is_male:= NA]
  if (change_names == TRUE) {
    setnames(toadd_agesex, old = c("AGE"), new = c("age"))
  }
  # Hmisc::describe(toadd_agesex)
  toadd_agesex[]
}

# utils::globalVariables(c("SEX", "sex.0_is_male"))
