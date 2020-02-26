#' replace sentinel values with NA
#'
#' @param vektor a vector of numeric or character values
#' @param missingCode a vector of values used as indicators of missing values
#' @return the input vector with the sentinel values replaced by NA
#' @noRd
removeMissingCodes = function(vektor, missingCode = c(-1,99)){
  message("Setting values ", paste(missingCode, collapse = ", "), " in ", deparse(substitute(vektor)), " to NA...")
  # vektor[stringr::str_trim(vektor) %in% missingCode] = NA
  vektor[as.numeric(vektor) %in% missingCode] = NA
  vektor
}
