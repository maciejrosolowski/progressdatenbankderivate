#' Get data on TUMOR, HI (heart insufficiency), CEREBROERK, CHRNIERE,
#' CHRLEBER and nurse.home.
#'
#' @param FRM_BAS data.table containing the table FRM_BAS
#' from the database of the PROGRESS study
#' @param change_names a logical value. If TRUE, then the names of the patient
#' characteristics will be changed. This is expected by the functions which
#' computed the severity scores.
#'
#' @return data.table with the ID of the patient (patstuid), and the
#' information on tumor status (TUMOR), heart insufficiency (HI),
#' CEREBROERK, chronic renal disease (CHRNIERE), chronic liver disease
#' (CHRLEBER), and the data on whether the patient is in a nursing home,
#' in the wide format.
#' @export
#'
#' @examples
#' \dontrun{
#' excel_fn <- paste0("/net/ifs1/san_projekte/projekte/",
#' "PROGRESS/Datenmanagement/Data_freezes/",
#' "20190320/PROGRESS-freeze_201903_01.xlsx")
#' FRM_BAS <- readxl::read_excel(excel_fn, 'FRM_BAS')
#' data.table::setDT(FRM_BAS)
#' toadd_tum.herz.cer.ren.liv.nurs <- getData4tum.herz.cer.ren.liv.nurs(FRM_BAS)
#' toadd_tum.herz.cer.ren.liv.nurs[]
#' toadd_tum.herz.cer.ren.liv.nurs <-
#' getData4tum.herz.cer.ren.liv.nurs(FRM_BAS, change_names = FALSE)
#' toadd_tum.herz.cer.ren.liv.nurs[]
#' }
getData4tum.herz.cer.ren.liv.nurs = function(FRM_BAS, change_names = TRUE) {
  # due to non-standard evaluation notes in R CMD check
  CEREBROERK <- CHRLEBER <- CHRNIERE <- HI <- PATSTUID <- TUMOR <- WOHNUNG <-
    nurse.home <- NULL
  toadd_tum.herz.cer.ren.liv.nurs =
    FRM_BAS[,.(PATSTUID, TUMOR, HI, CEREBROERK,
               CHRNIERE, CHRLEBER, nurse.home = WOHNUNG)]

  # 2020-03-21 MRosolowski
  # replace -1, 98, 99 by NA
  for (j in setdiff(colnames(toadd_tum.herz.cer.ren.liv.nurs), "PATSTUID")) {
    set(toadd_tum.herz.cer.ren.liv.nurs,
        which(toadd_tum.herz.cer.ren.liv.nurs[[j]] %in% c(-1, 98, 99)), j, NA)
  }
  # nursery home resident: WOHNUNG == 4
  toadd_tum.herz.cer.ren.liv.nurs[, nurse.home := nurse.home == 4]

  # # MRos: I commented out the originial code because it did not replace
  # -1, 98, 99 with NA
  # toadd_tum.herz.cer.ren.liv.nurs[nurse.home %in% c(-1, 98, 99), nurse.home:= NA]
  # setDF(toadd_tum.herz.cer.ren.liv.nurs)
  # toadd_tum.herz.cer.ren.liv.nurs[toadd_tum.herz.cer.ren.liv.nurs==-1] <- NA
  # toadd_tum.herz.cer.ren.liv.nurs[toadd_tum.herz.cer.ren.liv.nurs==98] <- NA
  # toadd_tum.herz.cer.ren.liv.nurs[toadd_tum.herz.cer.ren.liv.nurs==99] <- NA
  # setDT(toadd_tum.herz.cer.ren.liv.nurs)

  if (change_names == TRUE) {
    setnames(toadd_tum.herz.cer.ren.liv.nurs,
             old = c("PATSTUID", "TUMOR", "HI", "CEREBROERK", "CHRNIERE",
                     "CHRLEBER"),
             new = c("patstuid", "tumor", "herz", "cerebro", "renal", "liver"))
  }

  # Hmisc::describe(toadd_tum.herz.cer.ren.liv.nurs)
  toadd_tum.herz.cer.ren.liv.nurs
}
