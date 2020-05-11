#' Calulate the SIRS score
#'
#' @param temp.min Minimal temperature in Celsius
#' @param temp.max Maximal temperature in Celsius
#' @param hfrq.max Pulse in beats/min
#' @param afrq.max Respiratory rate in breaths/min
#' @param pco2 Arterial partial pressure of carbon dioxide in kPa
#' @param leuko_min Minimal WBC (white blood cell) count per cubic cm
#' @param leuko_max Maximal WBC (white blood cell) count per cubic cm
#' @param stkern.neutro Banded neutrophils count per microliter
#' @param smkern.neutro Segmented neutrophils count per microliter
#' @param verwirrt Altered mental status (confusion), yes 1, no 0
#' @param thrombo_min Minimal thrombocyte count per microlitre
#' @param thrombo.daybefore Minimal thrombocyte count per microlitre
#' on the previous day
#' @param oxi.ind Minimal value of the oxygenation (Horowitz) index in kPa
#' @param chr.lunge Chronic lung disease, yes 1, no 0
#' @param diur Diuresis in ml per day
#' @param gewicht Weight in kg
#' @param bemin Base excess
#' @param sysbp.min Systolic blood pressure in mmHg
#' @param map Mean arterial pressure in kPa
#' @param kate Treatment using catecholamines, zes 1, no 0
#'
#' @return A list with two compontens. The first, "infec.septic.servsept"
#' is a scalar or vector with three possible values representing the SIRS
#' criteria which the patient meets: 1: infected, 2: septic, 3: severe septic.
#'  The second, "shock" is a logical indicator if the septic shock criteria are
#'  met.
#' @export
#'
#' @examples
#' sirs <- sirs_simple(
#'   temp.min = 34,
#'   temp.max = 35,
#'   hfrq.max = 104,
#'   afrq.max = 20,
#'   pco2 = 4.1,
#'   leuko_min = 7800,
#'   leuko_max = 7900,
#'   stkern.neutro = 395,
#'   smkern.neutro = 4424,
#'   verwirrt = 1,
#'   thrombo_min = 216000,
#'   thrombo.daybefore = 205000,
#'   oxi.ind = 43.81,
#'   chr.lunge = 1,
#'   diur = 3120,
#'   gewicht = 70,
#'   bemin = 3.4,
#'   sysbp.min = 95,
#'   map = 10.44,
#'   kate = FALSE)
#'   sirs

sirs_simple <- function(temp.min, temp.max, hfrq.max, afrq.max, pco2,
                        leuko_min, leuko_max, stkern.neutro, smkern.neutro,
                        verwirrt, thrombo_min, thrombo.daybefore, oxi.ind,
                        chr.lunge, diur, gewicht, bemin, sysbp.min, map, kate){
  out <- sirs.day(temp.min, temp.max, hfrq.max, afrq.max,pco2, leuko_min,
                  leuko_max, stkern.neutro, smkern.neutro, verwirrt,
                  thrombo_min, thrombo.daybefore, oxi.ind, chr.lunge, diur,
                  gewicht, bemin, sysbp.min, map, kate)
  # number of patients
  N <- length(temp.min)
  overview <- rep(1, N)
  overview[out[[2]]$sepsis] <- 2
  overview[out[[2]]$schwere.sepsis] <- 3
  list(infec.septic.servsept = overview, shock = out[[2]]$septischer.schock)
}
