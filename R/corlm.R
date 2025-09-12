#'@export
corlm_ <- function(object, type = c("std", "inf"), ...) {
  type <- match.arg(type)
  mf <- model.frame(object)
  modelMatrix <- model.matrix(object)
  modelMatrix <- subset(modelMatrix, select = -`(Intercept)`)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- colnames(modelMatrix)

  y_ <- as.character(labels[[2]])
  df <- cbind(modelMatrix, mf[y_])
  varsUti <- c(usedVars_names, y_)

  alias <- c(paste0("x", (1:length(usedVars_names))), "y")

  colnames(df) <- alias

  if (type == "std") {
    cor_matrix <- cor(df)
  } else {
    cor_matrix <- ppcor::pcor(df)$estimate
  }

  cor_matrix <- cbind(varsUti, alias, cor_matrix)

  colnames(cor_matrix) <- c("Variáveis", "Alias", alias)
  rownames(cor_matrix) <- NULL

  return(cor_matrix)
}

#'@export
cor.aux <- function(x, y) {
  classx <- class(x)
  classy <- class(y)

  if (is.numeric(x) && is.numeric(y)) {
    return(cor(x, y))
  } else if (classx == "factor" && is.numeric(y)) {
    mod <- aov(y ~ x, data = data.frame(x, y))
    return(effectsize::eta_squared(mod, verbose = F)[[2]])
  } else if (is.numeric(x) && classy == "factor") {
    mod <- aov(x ~ y, data = data.frame(x, y))
    return(effectsize::eta_squared(mod, verbose = F)[[2]])
  } else if (classx == "factor" && classy == "factor") {
    tabela <- table(x, y)
    invisible(rcompanion::cramerV(tabela))
  } else {
    return(1)
  }
}

#'@export
corlm <- function(object, ...) {
  mf <- model.frame(object)

  modelMatrix <- model.matrix(object)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)

  y_ <- as.character(labels[[2]])
  varsUti <- c(usedVars_names, y_)
  mf <- mf[, varsUti]

  alias <- c(paste0("x", (1:length(usedVars_names))), "y")

  colnames(mf) <- alias

  n <- ncol(mf)
  tb <- matrix(0, n, n)

  for (i in 1:n) {
    for (j in 1:n) {
      tb[i, j] <- as.numeric(cor.aux(mf[[i]], mf[[j]]))
    }
  }

  tb <- cbind(varsUti, alias, tb)

  colnames(tb) <- c("Variáveis", "Alias", alias)

  return(tb)
}
