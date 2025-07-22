#' Fitting estate avaliation Linear Models
#'
#' @param formula an object of class "formula" (or one that
#' can be coerced to that class): a symbolic description of
#'  the model to be fitted..
#' @param y an optional data frame, list or environment (or object
#'  coercible by as.data.frame to a data frame) containing the
#' variables in the model. If not found in data, the variables are
#' taken from environment(formula), typically the environment from
#' which lm is called.
#' @returns estatelm return an object of class "estatelm".
#' @export
#' @examples
#' data(exemplo)
#' estatelm(preco ~ area + quartos + banheiros, data = exemplo)

estatelm <- function(
  formula,
  data,
  subset = NULL,
  transf = NULL,
  mode = c("rural", "urban"),
  ...
) {
  mode <- match.arg(mode)

  data_o <- data
  if (!is.null(transf)) {
    transformar_df <- function(df, transformacoes) {
      # Verificação de nomes válidos
      colunas <- names(transformacoes)
      # Aplica as transformações
      for (col in colunas) {
        funcao <- match.fun(transformacoes[[col]])
        df[[col]] <- funcao(df[[col]])
      }

      return(df)
    }

    transf_aux <- function(vetor, nome) {
      nome <- as.character(nome) # garante que são strings

      out <- vetor[nome] # tenta buscar os nomes
      out[is.na(out)] <- "I" # substitui os que não encontrou
      names(out) <- nome

      return(out)
    }

    transf <- transf_aux(transf, colnames(data))

    data <- transformar_df(data, transf)
  }

  model <- lm(formula, data, subset, ...)
  model$dataOrigin <- data_o
  model$transf <- transf
  model$mode <- mode
  # Modifica classe para personalizada
  class(model) <- c("estatelm", class(model))

  return(model)
}


#' @export
summary.estatelm <- function(object, ...) {
  call <- match.call()
  mf <- model.frame(object)
  allVars_names <- names(object$dataOrigin)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)
  allVars_num <- length(allVars_names)
  usedVars_num <- (length(usedVars_names) + 1) # add 1 for the response variable
  allObs_num <- nrow(object$dataOrigin)
  usedObs_num <- nrow(mf)

  tb <- data.frame(
    labels = c(
      "Total de variaveis:",
      "Variaveis Utilizadas:",
      "Total de dados:",
      "Dados utilizados no modelo:"
    ),
    values = c(allVars_num, usedVars_num, allObs_num, usedObs_num)
  )
  colnames(tb) <- c("Variaveis e dados do Modelo", "Quant.")

  res = list(tb = tb)
  class(res) <- "summary.estatelm"

  return(res)
}

#' @export
print.summary.estatelm <- function(x, ...) {
  cat("\n")
  print(x$tb)
}
