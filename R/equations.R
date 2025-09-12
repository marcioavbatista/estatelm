#'@export
transf.frame <- function(transf, vars) {
  output <- transf
  names(output) <- vars
  return(output)
}


#'@export
equation <- function(object, ...) {
  transf <- object$transf
  call <- match.call()
  mf <- model.frame(object)
  mm <- model.matrix(object)
  allVars_names <- names(object$dataOrigin)
  att <- attributes(object$terms)
  labels <- att$variables
  response_name <- as.character(labels[[2]])
  indepTerms_names <- colnames(mm)
  indepTerms_names <- as.character(indepTerms_names[
    indepTerms_names != "(Intercept)"
  ])
  coef <- coefficients(object)
  coef_ <- vector(length = length(coef))

  try(
    coef_ <- scales::number(
      coef["(Intercept)"],
      big.mark = "",
      decimal.mark = ",",
      accuracy = 10^-4
    ),
    silent = TRUE
  )

  coef_ <- c(
    coef_,
    scales::number(
      coef[indepTerms_names],
      style_positive = "plus",
      big.mark = "",
      decimal.mark = ",",
      accuracy = 10^-4
    )
  )

  if (is.null(transf)) {
    if (names(coef_)[1] == "(Intercept)") {
      rightEquation <- paste(
        coef_["(Intercept)"],
        paste(coef_[-1], indepTerms_names, collapse = " ")
      )
    } else {
      rightEquation <- paste(coef_, indepTerms_names, collapse = " ")
    }

    equation <- paste(response_name, " = ", rightEquation)
  } else {
    transfIndep <- transf[indepTerms_names]
    transfDep <- transf[response_name]

    transf_vars_names <- function(name, transf) {
      # Verificação simples
      name <- as.character(name)
      transf <- as.character(transf)
      if (length(name) != length(transf)) {
        stop("Os vetores 'name' e 'trans' devem ter o mesmo comprimento.")
      }

      # Usa mapply para vetorização elegante
      nomes_vari_transf <- mapply(
        function(n, t) {
          switch(
            t,
            "I" = n,
            "x" = n,
            "y" = n,
            "inverso" = paste0("1 / ", n),
            "log" = paste0("ln(", n, ")"),
            "log10" = paste0("log10(", n, ")"),
            "quadrado" = paste0(n, "²"),
            "cubo" = paste0(n, "³"),
            # caso padrão
            paste0("transf(", t, ")(", n, ")")
          )
        },
        name,
        transf,
        USE.NAMES = FALSE
      )

      return(nomes_vari_transf)
    }

    transNameIndep <- transf_vars_names(indepTerms_names, transfIndep)
    transNameDep <- transf_vars_names(response_name, transfDep)

    if (names(coef_)[1] == "(Intercept)") {
      rightEquation <- paste(
        coef_["(Intercept)"],
        paste(coef_[-1], transNameIndep, collapse = " ")
      )
    } else {
      rightEquation <- paste(coef_, transNameIndep, collapse = " ")
    }

    equation <- paste(transNameDep, " = ", rightEquation)
  }

  return(equation)
}
