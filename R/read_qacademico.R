#' @importFrom  dplyr %>%
#' @export
read_qacademico <- function(file = "extdata/Q_Academico_2009_1 a 2019_1.xlsx",
                            sheet = 1){
  openxlsx::read.xlsx(xlsxFile = file, sheet = sheet) %>%
    dplyr::as.tbl()
}
