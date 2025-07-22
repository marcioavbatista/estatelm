#'@export
corlm_ <- function(object, type = c("std", "inf"), ...) {
  type <- match.arg(type)

  df <- object$dataOrigin
  modelMatrix <- model.matrix(object)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)

  y_ <- as.character(labels[[2]])
  varsUti <- c(usedVars_names, y_)
  df <- df[, varsUti]

  alias <- c(paste0("x", (1:length(usedVars_names))), "y")

  colnames(df) <- alias

  if (type == "std") {
    cor_matrix <- cor(df)
  } else {
    cor_matrix <- ppcor::pcor(df)$estimate
  }

  cor_matrix <- cbind(varsUti, alias, cor_matrix)

  colnames(cor_matrix) <- c("Variáveis", "Alias", alias)

  return(cor_matrix)
}

#'@export
cor.aux <- function(x, y) {
  classx <- class(x)
  classy <- class(y)

  if (classx == "numeric" && classy == "numeric") {
    return(cor(x, y))
  } else if (classx == "factor" && classy == "numeric") {
    mod <- aov(y ~ x, data = data.frame(x, y))
    return(effectsize::eta_squared(mod, verbose = F)[[2]])
  } else if (classx == "numeric" && classy == "factor") {
    mod <- aov(x ~ y, data = data.frame(x, y))
    return(effectsize::eta_squared(mod, verbose = F)[[2]])
  } else if (classx == "factor" && classy == "factor") {
    tabela <- table(x, y)
    invisible(rcompanion::cramerV(tabela))
  }
}

#'@export
corlm <- function(object, ...) {
  df <- object$dataOrigin
  modelMatrix <- model.matrix(object)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)

  y_ <- as.character(labels[[2]])
  varsUti <- c(usedVars_names, y_)
  df <- df[, varsUti]

  alias <- c(paste0("x", (1:length(usedVars_names))), "y")

  colnames(df) <- alias

  n <- ncol(df)
  tb <- matrix(0, n, n)

  for (i in 1:n) {
    for (j in 1:n) {
      tb[i, j] <- cor.aux(df[[i]], df[[j]])
    }
  }

  tb <- cbind(varsUti, alias, tb)

  colnames(tb) <- c("Variáveis", "Alias", alias)

  return(tb)
}
