#' get data on the presence of the chronic lung disease
#'
#' @param FRM_BAS data.table containing the table FRM_BAS from the database
#' of the PROGRESS study
#' @return data.table. Each row corresponds to one patient
#' and the columns contain information on the presence (1) or absence (0)
#' of the chronic lung disease.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BAS <- readxl::read_excel(excel_fn, "FRM_BAS")
#' data.table::setDT(FRM_BAS)
#' getData4chr.lunge(FRM_BAS)
#' }
getData4chr.lunge = function(FRM_BAS) {
  # due to non-standard evaluation notes in R CMD check
  CHRATEMLUNG <- PATSTUID <- chr.lunge <- NULL
  # 48 Chronische Lunge/Atemwegserkrankung
  toadd_chr.lunge<-FRM_BAS[,.(patstuid = PATSTUID, chr.lunge=CHRATEMLUNG )]
  toadd_chr.lunge[chr.lunge  %in% c(-1,99),chr.lunge :=NA]
  toadd_chr.lunge[]
}
