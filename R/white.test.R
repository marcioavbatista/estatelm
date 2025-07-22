#'@export
white.test <- function(object, ...) {
  call <- match.call()
  mf <- model.frame(object)
  mm <- model.matrix(object)

  k <- ncol(mm) - 1 # - intercept
  n <- nrow(mm)
  X <- matrix(0, nrow = n, ncol = ((k * (k + 1)) / 2) + 1)
  iter <- 1
  for (i in 2:k) {
    for (j in i:k) {
      X[, iter] <- mm[, j] * mm[, i]
      iter = iter + 1
    }
  }

  X <- cbind(1, X)
  Y <- model.response(mf)
  lm_aux <- lm.fit(x = X, y = Y)
  p <- lm_aux$rank # número de coeficientes estimados
  rss <- sum(lm_aux$residuals^2)
  tss <- sum((Y - mean(Y))^2)
  r2_adj <- 1 - (rss / (n - p)) / (tss / (n - 1))

  df <- (k * (k + 1)) / 2
  stat <- n * r2_adj
  p.value <- pchisq(stat, df = df, lower.tail = F)

  output <- list(stat = stat, df = df, p.value = p.value)
  class(output) <- "white.test"
  return(output)
}

#'@export
print.white.test <- function(x, ...) {
  cat("Estatística do teste de White:", x$stat, "\n")
  cat("Graus de liberdade:", x$df, "\n")
  cat("p-valor:", x$p.value, "\n")
}

#'@export
summary.white.test <- function(x, ...) {
  tb <- data.frame(
    "Estatística do teste de White:" = x$stat,
    "Graus de liberdade:" = x$df,
    "p-valor:" = x$p.value
  )
  return(tb)
}
