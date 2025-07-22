#'@export
residuals_table <- function(object, ...) {
  std <- sqrt(qme(object))

  res <- residuals(object)
  fitted <- fitted(object)
  ind <- names(res)
  dcook <- cooks.distance(object)

  tb <- data.frame(
    obs = res + fitted,
    fit = fitted,
    res = res,
    resP = scales::percent(res / (res + fitted)),
    resDp = res / std,
    dcook = dcook
  )

  tb <- cbind(ind, tb)

  colnames(tb) <- c(
    "Dado",
    "Observado",
    "Estimado",
    "Resíduo",
    "Resíduo (%)",
    "Resíduo / DP",
    "DCook"
  )

  return(tb)
}
