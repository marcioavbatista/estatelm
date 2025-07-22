#'@export
sig.test <- function(object, ...) {
  class(object) <- "lm"
  summa <- summary(object)

  allVars_names <- names(object$dataOrigin)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)

  coef <- summa$coefficients

  tb <- data.frame(
    t = coef[, 3],
    p = coef[, 4]
  )
  rownames(tb)[1] <- "Intercepto"
  output <- list(tb = tb)
  class(output) <- "sig.test"

  return(output)
}


#'@export
print.sig.test <- function(x, ...) {
  cat("\n")
  print(x$tb)
}
