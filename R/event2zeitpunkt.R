#' map between the column EVENTref and the columns zeitpunktref or zp_fabianref
#' in event2zeitpunkt_table. These columns are different labels of the time points
#'
#' @param EVENT vector of characters. They must be present in
#' event2zeitpunkt_table$EVENTref.
#' @param returnformat a character string indicating whether zeitpunktref or
#' zp_fabianref should be returned.
#' @param event2zeitpunkt_df data.table event2zeitpunkt_table (available with
#' the package).
#'
#' @return a vector of labels of the type zeitpunktref or zp_fabianref
#' @export
#'
#' @examples
#' event2zeitpunkt(EVENT = c(1, 2, 31, 8))
#' @importFrom stats na.omit
event2zeitpunkt = function(EVENT, returnformat = c("zeitpunktref", "zp_fabianref"),
                           event2zeitpunkt_df =
                             progressdatenbankderivate::event2zeitpunkt_table) {

  if(all(na.omit(EVENT) %in% event2zeitpunkt_df$EVENTref)==F)
    stop("Following event number(s) undefined in this function: ",
         na.omit(EVENT)[!(na.omit(EVENT) %in% event2zeitpunkt_df$EVENTref)] %>% unique)

  if (anyDuplicated(na.omit(event2zeitpunkt_df$EVENTref)) != 0) {
    stop("duplicated elements in event2zeitpunkt_table$EVENTref")
  }

  if(returnformat == "zeitpunktref") {
    # EVENTzp = event2zeitpunkt_df[toolboxH::match_hk(EVENT, event2zeitpunkt_df$EVENTref),zeitpunktref]
    # alternative code, not using the R package toolboxH
    EVENTzp <- event2zeitpunkt_df[match(EVENT, event2zeitpunkt_df$EVENTref), zeitpunktref]
    return(EVENTzp)} else  if(returnformat == "zp_fabianref") {
      # EVENTzp = event2zeitpunkt_df[toolboxH::match_hk(EVENT, event2zeitpunkt_df$EVENTref),zp_fabianref]
      # alternative code, not using the R package toolboxH
      EVENTzp <- event2zeitpunkt_df[match(EVENT, event2zeitpunkt_df$EVENTref), zp_fabianref]
      return(EVENTzp) } else stop("returnformat must be either 'zeitpunktref' or 'zp_fabianref' ")
}

utils::globalVariables(c("zp_fabianref"))
