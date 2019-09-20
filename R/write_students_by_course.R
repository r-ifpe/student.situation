#' @importFrom dplyr %>%
#' @export
write_students_by_course <- function(x, courses,
                                     years = 2009:2019){

  alunos_curso <- purrr::map(1:nrow(courses), function(e){
    x %>%
      dplyr::filter(desc_curso == courses$Curso[e],
                    Desc_Instituicao == courses$Campus[e]) %>%
      dplyr::transmute(Nome_Aluno = Nome_Pessoa,
                       Matricula = Matricula,
                       Situacao = Desc_Sit_Matricula,
                       Curso = desc_curso,
                       Campus = Desc_Instituicao,
                       Turno = stringr::str_to_upper(Desc_Turno),
                       Modalidade_Curso = stringr::str_to_upper(Desc_Modalidade_Curso),
                       Ano = substr(Matricula, 1, 4),
                       Semestre = substr(Matricula, 5, 5))
  })

  lapply(alunos_curso, write_students, years)
}

# write a xlsx with year.period by sheet
write_students <- function(x, years){
  campus <- x$Campus[1] %>% stringr::str_remove("IFPE / ")
  curso <- x$Curso[1]
  file <- paste0(campus, "_", curso, ".xlsx")
  t <- expand.grid(years, 1:2) %>% dplyr::arrange(Var1)

  alunos_por_planilha <- list()
  for(e in 1:nrow(t)){
    alunos <- x %>%
      dplyr::filter(Ano == t$Var1[e], Semestre == t$Var2[e]) %>%
      dplyr::select(Nome_Aluno, Matricula, Situacao, Turno, Modalidade_Curso) %>%
      dplyr::arrange(Turno, Modalidade_Curso, Nome_Aluno)

    if(nrow(alunos) > 0 ){
      alunos_por_planilha[[paste0(t$Var1[e], ".", t$Var2[e])]] = alunos
    }
  }

  openxlsx::write.xlsx(alunos_por_planilha, file = file)
}

