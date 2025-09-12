#' @export
ground <- function(
  object,
  newdata,
  char = c("III", "II", "I"),
  id = c("III", "II", "I"),
  ...
) {
  char <- match.arg(char)
  id <- match.arg(id)
  mm <- as.data.frame(model.matrix(formula(object), object$dataOrigin)[, -1])
  mf <- model.frame(object)[, 2:ncol(model.frame(object))]
  allVars_names <- names(object$dataOrigin)
  att <- attributes(object$terms)
  labels <- att$variables
  usedVars_names <- with(att, term.labels)
  allVars_num <- length(allVars_names)
  usedVars_num <- length(usedVars_names) + 1
  allObs_num <- nrow(object$dataOrigin)
  usedObs_num <- nrow(mm)
  mode <- object$mode

  newdata <- as.data.frame(model.matrix(
    update(formula(object), NULL ~ .),
    newdata
  )[, -1, drop = FALSE])

  groundVector <- matrix(0, nrow = nrow(newdata), ncol = 6)

  groundVector[, 1] <- rep(
    switch(char, III = 3, II = 2, I = 1, 0),
    nrow(newdata)
  )
  groundVector[, 3] <- rep(switch(id, III = 3, II = 2, I = 1, 0), nrow(newdata))

  k <- usedVars_num
  n <- usedObs_num

  # Validade amostral (coluna 2)
  if (mode == "rural") {
    groundVector[, 2] <- dplyr::case_when(
      n >= 4 * (k + 1) ~ 3,
      n >= 3 * (k + 1) ~ 2,
      n >= 2 * (k + 1) ~ 1,
      TRUE ~ 0
    )
  } else {
    groundVector[, 2] <- dplyr::case_when(
      n >= 6 * (k + 1) ~ 3,
      n >= 4 * (k + 1) ~ 2,
      n >= 3 * (k + 1) ~ 1,
      TRUE ~ 0
    )
  }

  # Função auxiliar
  classificar_valor <- function(x, v_ref) {
    ifelse(x > max(v_ref), 1, ifelse(x < min(v_ref), -1, 0))
  }

  esc_aux <- matrix(0, nrow = nrow(newdata), ncol = ncol(mm))
  newdata_aux <- matrix(0, nrow = nrow(newdata), ncol = ncol(mm))
  colnames(esc_aux) <- colnames(mm)
  colnames(newdata_aux) <- colnames(mm)

  for (j in 1:nrow(newdata)) {
    new <- newdata[j, ]

    for (i in colnames(mm)) {
      vet_aux <- as.numeric(mm[[i]])
      idx <- which(colnames(mm) == i)
      val <- as.numeric(new[[i]])

      if (is.numeric(vet_aux)) {
        esc_aux[j, idx] <- classificar_valor(val, vet_aux)
      } else {
        esc_aux[j, idx] <- 0
      }
    }

    if (all(esc_aux[j, ] == 0)) {
      groundVector[j, 4] <- 3
    } else {
      extrapolou <- FALSE
      for (i in colnames(mm)) {
        idx <- which(colnames(mm) == i)
        val <- as.numeric(new[[i]])
        vet_ref <- as.numeric(mm[[i]])

        if (esc_aux[j, idx] == 0) {
          newdata_aux[j, idx] <- val
        } else if (esc_aux[j, idx] == -1) {
          if (val >= 0.5 * min(vet_ref)) {
            newdata_aux[j, idx] <- min(vet_ref)
          } else {
            groundVector[j, 4] <- 0
            extrapolou <- TRUE
            break
          }
        } else if (esc_aux[j, idx] == 1) {
          if (val <= 2 * max(vet_ref)) {
            newdata_aux[j, idx] <- max(vet_ref)
          } else {
            groundVector[j, 4] <- 0
            extrapolou <- TRUE
            break
          }
        }
      }

      if (!extrapolou) {
        pred_aux <- predict(object, as.data.frame(newdata_aux))
        pred <- predict(object, newdata)
        ratios <- pred[, 1] / pred_aux[, 1]

        graus_aux <- if (mode == "rural") {
          dplyr::case_when(
            abs(ratios) > 0.30 ~ 1,
            abs(ratios) > 0.20 ~ 2,
            TRUE ~ 0
          )
        } else {
          dplyr::case_when(
            abs(ratios) > 0.20 ~ 1,
            abs(ratios) > 0.15 ~ 2,
            TRUE ~ 0
          )
        }

        groundVector[j, 4] <- graus_aux
      }
    }
  }

  # Significância estatística (coluna 5)
  summa <- summary.lm(object)
  p_t <- summa$coefficients[, 4]
  groundVector[, 5] <- dplyr::case_when(
    max(p_t) <= 0.1 ~ 3,
    max(p_t) <= 0.2 ~ 2,
    max(p_t) <= 0.3 ~ 1,
    TRUE ~ 0
  )

  # Significância F-global (coluna 6)
  aov <- anova(object)
  sq <- aov$`Sum Sq`
  df <- aov$Df
  tam <- length(sq)

  f <- (sum(sq[-tam]) / sum(df[-tam])) / (sq[tam] / df[tam])
  p_f <- pf(f, df1 = sum(df[-tam]), df2 = df[tam], lower.tail = FALSE)

  groundVector[, 6] <- dplyr::case_when(
    p_f <= 0.01 ~ 3,
    p_f <= 0.02 ~ 2,
    p_f <= 0.05 ~ 1,
    TRUE ~ 0
  )

  # Combinação das pontuações em um vetor final `aux`
  aux <- numeric(nrow(newdata))

  for (i in 1:nrow(newdata)) {
    score_min <- min(groundVector[i, c(2, 5, 6)])
    score_sum <- sum(groundVector[i, ])

    aux[i] <- dplyr::case_when(
      score_min <= 0 ~ 0,
      score_min <= 1 & score_sum >= 6 ~ 1,
      score_min <= 2 & score_sum >= 9 ~ 2,
      score_min <= 3 & score_sum >= 15 ~ 3,
      score_min <= 3 & score_sum >= 12 ~ 2,
      TRUE ~ 0
    )
  }

  return(list(aux = aux, detail = groundVector, newdata, esc_aux))
}
