#' @export
anovaModel <- function(object) {
  class(object) <- "lm"
  anova <- car::Anova(object)

  allVars_names <- names(object$dataOrigin)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)

  sq_ <- anova[[1]]
  gl_ <- anova[[2]]
  qm_ <- sq_ / gl_
  f_ <- anova[[3]]
  p_ <- anova[[4]]

  tam_anova <- nrow(anova)

  # adapt the coefficients of the table into explained and not explained

  # creates the adapted anova table
  tb <- data.frame(
    c(usedVars_names, "Resíduos"),
    gl_,
    sq_,
    qm_,
    f_,
    p_
  )

  # defines columns names for the tables
  colnames(tb) <- c(
    "Fonte de Variação",
    "Graus de Liberdade",
    "Soma dos Quadrados",
    "Quadrado Médio",
    "F",
    "p-valor"
  )

  output <- list(tb = tb)

  class(output) <- "anovaModel"
  return(output)
}


#' @export
print.anovaModel <- function(x, ...) {
  cat("\n")
  print(x$tb)
}
