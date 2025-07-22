#'@export
estateSummary <- function(object, ...) {
  class(object) <- "lm"
  sum <- summary(object)
  r2 <- sum$r.squared
  adj_r2 <- sum$adj.r.squared

  aov <- anova(object)
  sq <- aov$`Sum Sq`
  df <- aov$Df
  tam <- length(sq)

  f <- (sum(sq[-tam]) / sum(df[-tam])) / (sq[tam] / df[tam])
  p.value <- pf(f, df1 = sum(df[-tam]), df2 = df[tam], lower.tail = F)

  labs <- c(
    "Coeficiente de determinação R2:",
    "Coeficiente de determinação ajustado:",
    "Fisher - Snedecor:",
    "Significancia do modelo(%):"
  )

  values <- c(
    r2,
    adj_r2,
    f,
    scales::percent(p.value, accuracy = 10^-4, decimal.mark = ",")
  )

  # creates df with the measures and their titles
  df_sumario <- data.frame(labs, values)

  colnames(df_sumario) <- c("Estatísticas do modelo", "Estatística")

  return(df_sumario)
}
