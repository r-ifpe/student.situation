#' @importFrom dplyr %>%
#' @export
ifpe_courses <- function(x){
  x %>%
    dplyr::transmute(Curso = desc_curso,
                     Campus = Desc_Instituicao) %>%
    dplyr::distinct()
}
