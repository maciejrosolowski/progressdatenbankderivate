#' get data on KATECHO
#'
#' Values other then 1 are interpreted as 0.
#'
#' @param FRM_KAT data.table containing the table FRM_KAT from the database
#' of the PROGRESS study
#'
#' @return data.table. Each row corresponds one patient, identified by
#' the column "patstuid". Columns "kat.atall", "kat.d0", "kat.ab.d1" are
#' logical and indicate, if KATECHO == 1 at any time, at d0 and after d0.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_KAT <- readxl::read_excel(excel_fn, "FRM_KAT")
#' data.table::setDT(FRM_KAT)
#' toadd_kate <- getData4kate(FRM_KAT)
#' toadd_kate
#' }
getData4kate <- function(FRM_KAT) {
  # due to non-standard evaluation notes in R CMD check
  EVENT <- PATSTUID <- KATECHO <- event <- patstuid <- value <-
    kat.atall <- kat.d0 <- kat.ab.d1 <- NULL
  # 51. kate (Basenueberschuss)
  kate = FRM_KAT[,.(patstuid = PATSTUID, event = EVENT,value= as.numeric(KATECHO))]
  kate[value == -1, value:=NA]
  # kate[,.N, value]
  kate[, value:=ifelse(value == 1, TRUE, NA)]
  kate[is.na(value)==T, value := FALSE]
  # Hmisc::describe(kate)

  kat.atall <- kate[, .(kat.atall=any(value ==TRUE)), patstuid]
  kat.d0    <- kate[, .(kat.d0=value[event==3] ==TRUE), patstuid]
  kat.ab.d1 <- kate[, .(kat.ab.d1=any(value[event!=3] ==TRUE)), patstuid]

  toadd_kate  = merge(kat.atall, kat.d0, by = "patstuid", all  =T ,sort = F)
  toadd_kate  = merge(toadd_kate, kat.ab.d1, by = "patstuid", all  =T ,sort = F)

  # stopifnot(nrow(toadd_kate[allDuplicatedEntries(patstuid)])==0)
  stopifnot(anyDuplicated(toadd_kate[, patstuid]) == 0)
  toadd_kate
}
