#' get the CURB65 score from the table DID_CURB65
#'
#' @param DID_CURB65 data.table containing the table DID_CURB65 from the database
#' of the PROGRESS study
#'
#' @return data.table of DID_CURB65 with -1 replaced by NA and added CRB65 column
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' DID_CURB65 <- readxl::read_excel(excel_fn, 'DID_CURB65')
#' data.table::setDT(DID_CURB65)
#' curb <- getData4curb(DID_CURB65)
#' curb[]
#' }
getData4curb = function(DID_CURB65) {
  curb = copy(DID_CURB65)
  # due to non-standard evaluation notes in R CMD check
  PATSTUID <- EVENT <- event <- AGE <- CRB <- CRB65 <- CURB65 <- NULL
  # stopifnot(nrow(curb[allDuplicatedEntries(paste(PATSTUID, EVENT))])==0)
  stopifnot(anyDuplicated(curb, by = c("PATSTUID", "EVENT")) == 0)

  # replace -1 with NA
  # setDF(curb)
  # curb[curb==-1] = NA
  # setDT(curb)
  for(j in seq_len(ncol(curb))) {
    set(curb, which(curb[[j]] == -1), j, value = NA)
  }

  # showNA(curb)
  curb[,CRB65 := sum(CRB, AGE, na.rm = T), .(PATSTUID, EVENT)]
  # curb[is.na(CRB)]
  curb$PATID_EXT = NULL
  erg <- list()
  erg$input <- curb
  curb <- curb[, .(patstuid = PATSTUID, event = EVENT, crb65 = CRB65,
                   curb65 = CURB65)]
  curb[, event := event2zeitpunkt(event, returnformat = "zp_fabianref")]
  curb <- dcast(curb, patstuid ~ event, value.var = c("crb65", "curb65"))
  erg$out <- curb
  erg
}

# utils::globalVariables(c("PATSTUID", "EVENT", "AGE", "CRB", "CRB65", "."))
