#' Calculate the PSI score
#'
#' @param age Age
#' @param verwirrt Altered mental status (confusion), yes 1, no 0
#' @param hfrq.max Pulse in beats/min
#' @param afrq.max Respiratory rate in breaths/min
#' @param sysbp.min Systolic blood pressure in mmHg
#' @param temp.min Minimal temperature in Celsius
#' @param temp.max Maximal temperature in Celsius
#' @param tumor Neoplastic disease, yes 1, no 0
#' @param herz Congestive heart failure history, yes 1, no 0
#' @param cerebro Cerebrovascular disease history
#' @param renal Renal disease history
#' @param liver Liver disease history
#' @param gender sex, male 0, female 1
#' @param nurse.home Nursing home resident, yes 1, no 0
#' @param art.ph.min pH
#' @param bun BUN in mmol/L
#' @param snat Snat (Sodium) in mmol/L
#' @param gluk Glucose in mmol/L
#' @param haemkrt Hematocrit in percent
#' @param apo2.min Partial pressure of oxygen in mmHg
#' @param pleu_erg Pleural effusion on x-ray
#'
#' @return The value of the PSI score (positive integer).
#' @export
#'
#' @examples
#' psi <- psi_simple(
#'   age = 68,
#'   verwirrt = 1,
#'   hfrq.max = 130,
#'   afrq.max = 35,
#'   sysbp.min = 80,
#'   temp.min = 34,
#'   temp.max = 35,
#'   tumor = 1,
#'   herz = 1,
#'   cerebro = 1,
#'   renal = 1,
#'   liver = 1,
#'   gender = 1,
#'   nurse.home = 1,
#'   art.ph.min = 7.2,
#'   bun = 10,
#'   snat = 120,
#'   gluk = 16,
#'   haemkrt = 0.2,
#'   apo2.min = 50,
#'   pleu_erg = 1)
#'   psi

psi_simple <- function(age, verwirrt, hfrq.max, afrq.max, sysbp.min, temp.min,
                       temp.max, tumor, herz, cerebro, renal,liver,
                       gender, nurse.home, art.ph.min, bun, snat, gluk, haemkrt,
                       apo2.min,pleu_erg) {
  psi.I.to.V(age, verwirrt, hfrq.max, afrq.max, sysbp.min, temp.min, temp.max,
                    tumor, herz, cerebro, renal,liver,
                    gender, nurse.home, art.ph.min, bun, snat, gluk, haemkrt,
                    apo2.min, pleu_erg)$psi
}
