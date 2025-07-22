#' @export
descrNumData <- function(data = data, vars) {
  # is the data of the inicial data (before transform the variables)
  data <- data |> dplyr::select(vars)

  # creates a vector with the statistics (mean,max,min and ampli) of each column of the data.frame
  v_mean <- apply(data, mean, MARGIN = 2)
  v_max <- apply(data, max, MARGIN = 2)
  v_min <- apply(data, min, MARGIN = 2)
  v_ampli <- v_max - v_min

  # creates a data.frame with all the statistics
  df_descr <- data.frame(
    v_min,
    v_max,
    v_mean,
    v_ampli
  )

  colnames(df_descr) = c("Mínimo", "Máximo", "Média", "Amplitude")

  return(df_descr)
}


#' @export
descrFactorData <- function(data = data, vars) {
  data <- data |> dplyr::select(vars)

  freq_relativa_fator <- function(fator) {
    tab <- table(fator)
    tibble::tibble(
      nivel = names(tab),
      freq_rel = as.numeric(tab)
    )
  }

  # Aplicar a função a cada coluna fator
  resultados <- data |>
    purrr::map_dfr(
      .f = freq_relativa_fator,
      .id = "variavel"
    )
  return(resultados)
}
