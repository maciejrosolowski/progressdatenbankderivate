#' Compute the Halm score.
#'
#' @param hfrq.max Pulse in beats/min
#' @param sysbp.min Systolic blood pressure in mmHg
#' @param afrq.max Respiratory rate in breaths/min
#' @param o2p.min Minimum oxygen saturation in percent
#' @param apo2.min Partial pressure of oxygen in mmHg
#' @param bea mechanical ventilation, yes 1, no 0.
#' @param sauerst oxygen therapy, yes 1, no 0.
#' @param temp.max Maximal temperature in Celsius
#' @param verwirrt Altered mental status (confusion), yes 1, no 0.
#' @param gcs Glasgow Coma Scale
#'
#' @return The value of the Halm score.
#' @export
#'
#' @examples
#' halm <- HalmScore_simple(
#' hfrq.max = 104,
#' sysbp.min = 95,
#' afrq.max = 19,
#' o2p.min = 90,
#' apo2.min = 50,
#' bea = 1,
#' sauerst = 0,
#' temp.max = 38,
#' verwirrt = 0,
#' gcs = 15)
#' halm

HalmScore_simple <- function(hfrq.max, sysbp.min, afrq.max, o2p.min,
                             apo2.min, bea, sauerst, temp.max, verwirrt, gcs) {
  HRF.p <- as.numeric(hfrq.max > 100)
  SBP.p <- as.numeric(sysbp.min < 90)
  AF.p <- as.numeric(afrq.max > 24)
  O2.p <- as.numeric(o2p.min < 90 | (apo2.min < 60) | bea | sauerst)
  KT.p <- as.numeric(temp.max > 37.8)
  MS.p <- as.numeric(verwirrt | (gcs < 15))

  #Gesamtscore berechnen
  dummy<-cbind(HRF.p,SBP.p,AF.p,O2.p,KT.p,MS.p)
  halm<-apply(dummy,1,function(x) sum(x,na.rm=T))
  halm
}
