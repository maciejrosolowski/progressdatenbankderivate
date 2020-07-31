#' Get data on antibiosis before hospital admission (ABIOT), chronic bed
#' confinement (CHRBETTL).
#'
#' @param FRM_BAS data.table containing the table FRM_BAS
#' from the database of the PROGRESS study
#'
#' @return data.table with the ID of the patient (PATSTUID),
#' time point (EVENT), and the information on antibiosis before hospital
#' admission (ABIOT), chronic bed confinement (CHRBETTL). Table FRM_BAS
#' contains values at EVENT == 3 at present.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BAS <- readxl::read_excel(excel_fn, 'FRM_BAS')
#' data.table::setDT(FRM_BAS)
#' res <- getData4abiot_chrbettl(FRM_BAS)
#' res[]
#' }
getData4abiot_chrbettl <- function(FRM_BAS) {
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- ABIOT <- CHRBETTL <- NULL
  out <- FRM_BAS[,.(PATSTUID, EVENT, ABIOT, CHRBETTL)]

  # replace -1, 98, 99 by NA
  for (j in setdiff(colnames(out), "PATSTUID")) {
    set(out, which(out[[j]] %in% c(-1, 98, 99)), j, NA)
  }
  out
}
