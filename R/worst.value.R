#' parallel worse values
#'
#' @param dum1 a vector of numeric values
#' @param dum2 a vector of numeric values
#' @param variable a character string, the name of the variable the values of
#' which are in the vectors dum1 and dum2.
#' @return the vector of parallel worse values of dum1 and dum2 (minima or
#' maxima depending on the variable)
#' @noRd
worst.value <- function(dum1,dum2,variable){
  min.set <- c("leuko_min","map","sysbp.min","bemin","diur","oxi.ind",
               "thrombo_min","pco2","sysbp.min","temp.min","art.ph.min","snat",
               "haemkrt","apo2.min","diasbp.min")
  max.set <- c("leuko_max","kate","chr.lunge","verwirrt","hfrq.max","afrq.max",
               "temp.max","bun","gluk","pleu_erg", "patbea")

  if (variable %in% min.set){
    dum<-apply (cbind(dum1,dum2),1,function(x){
      suppressWarnings(min(x,na.rm = T))} )
  }
  if (variable %in% max.set){
    dum<-apply (cbind(dum1,dum2),1,function(x){
      suppressWarnings(max(x,na.rm = T))} )
  }
  dum[dum %in% c(Inf,-Inf)]<-NA
  dum
}
