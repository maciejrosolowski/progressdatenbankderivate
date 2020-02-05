#' map between the column zp_fabianref and the columns EVENTref or zeitpunktref
#' in event2zeitpunkt_table. These columns are different labels of the time points
#'
#' @param zp_fabian vector of characters. They must be present in
#' event2zeitpunkt_table$zp_fabianref.
#' @param returnformat a character string indicating whether EVENTref or
#' zeitpunktref should be returned.
#' @param event2zeitpunkt_df data.table event2zeitpunkt_table (available with
#' the package).
#'
#' @return character vector of EVENTref or zeitpunktref
#' @export
#'
#' @examples
#' zeitpunkt2event(zp_fabian = c("auf", "d1", "d5.1"))
#' @importFrom stats na.omit
zeitpunkt2event = function(zp_fabian, returnformat = c("EVENTref", "zeitpunktref"),
                           event2zeitpunkt_df =
                             progressdatenbankderivate::event2zeitpunkt_table) {
  # EVENT=3

  if(all(na.omit(zp_fabian) %in% event2zeitpunkt_df$zp_fabianref)==F)
    stop("Following event number(s) undefined in this function: ",
         na.omit(zp_fabian)[!(na.omit(zp_fabian) %in% event2zeitpunkt_df$zp_fabianref)] %>% unique)

  if (anyDuplicated(na.omit(event2zeitpunkt_df$zp_fabianref)) != 0) {
    stop("duplicated elements in event2zeitpunkt_df$zp_fabianref")
  }

  if(returnformat == "EVENTref") {
    # EVENTref = event2zeitpunkt_df[toolboxH::match_hk(zp_fabian, event2zeitpunkt_df$zp_fabianref),EVENTref]
    # alternative code, not using the R package toolboxH
    EVENTref <- event2zeitpunkt_df[match(zp_fabian, event2zeitpunkt_df$zp_fabianref), EVENTref]
    return(EVENTref)} else  if(returnformat == "zeitpunktref") {
      # zeitpunktref = event2zeitpunkt_df[toolboxH::match_hk(zp_fabian, event2zeitpunkt_df$zp_fabianref),zeitpunktref]
      # alternative code, not using the R package toolboxH
      zeitpunktref <- event2zeitpunkt_df[match(zp_fabian, event2zeitpunkt_df$zp_fabianref), zeitpunktref]
      return(zeitpunktref) } else stop("returnformat must be either 'EVENTref' or 'zeitpunktref' ")
}
