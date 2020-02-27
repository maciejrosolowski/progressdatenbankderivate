#' get data about smoking behavior from the table FRM_BAS
#'
#' @param FRM_BAS data.table containing the table FRM_BAS from the
#' database of the PROGRESS study
#' @param rauchvars a character vector with the names of the variables
#'  to be extracted.
#'
#' @return a data.table with the PATSTUID and the extracted patient
#' characteristics.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BAS <- readxl::read_excel(excel_fn, "FRM_BAS")
#' data.table::setDT(FRM_BAS)
#' extractRauchstatus(FRM_BAS, rauchvars = "RAUCHL12M")
#' }
extractRauchstatus = function(FRM_BAS,
                              rauchvars = c('RAUCHJAHRE', 'RAUCHPACK',
                                            'RAUCHAKT', 'RAUCHL12M')) {
  # rauchvars = c('RAUCHJAHRE', 'RAUCHPACK','RAUCHAKT', 'RAUCHL12M')

  for(i in rauchvars) {
    FRM_BAS[,(i):= removeMissingCodes(get(i))]
  }
  FRM_BAS[,c("PATSTUID", rauchvars), with = F]
}
