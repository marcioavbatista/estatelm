#'@export
outliers_table <- function(object, ...) {
  res <- residuals(object)
  qme <- qme(object)

  resStd <- res / sqrt(qme)

  outNum <- sum(resStd <= -2 | resStd >= 2)

  propOut <- outNum / length(resStd)

  df_outliers <- data.frame(
    c("Quantidade de outliers:", "% de outliers:"),
    c(
      scales::number(outNum),
      scales::percent(propOut, accuracy = 0.01, decimal.mark = ",")
    )
  )
  colnames(df_outliers) <- NULL
  rownames(df_outliers) <- NULL

  return(df_outliers)
}
