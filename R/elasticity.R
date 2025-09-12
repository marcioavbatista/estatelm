#'@export
eslaticity <- function(object, ...) {
  # df is the inicial data
  # modelo is an lm object
  # trans is a sting vector with the used transformation in the model
  # vari is a sting vector with the names of the used variables

  # iniciates a list object that later will receive the plots generated
  lista <- list()
  subset <- eval(object$call$subset, envir = parent.frame())
  dataObs <- object$dataOrigin
  mm <- model.frame(object)[, -1]
  mmObs <- dataObs[, all.vars(formula(object)[[3]])]
  att <- attributes(object$terms)
  labels <- att$variables
  vari_dep <- labels[[2]]
  usedVars_names <- as.character(with(att, term.labels))

  # sets the estimated values
  predicts <- predict(
    object,
    dataObs[, all.vars(formula(object)[[3]])]
  )$PRED[, 1]
  # transform the sring vector to a list of functions

  mmRes <- mmObs %>% dplyr::summarise_all(mean)
  mmRes <- mmRes[rep(1, 200), ]

  # iterates betwen each independent variable and genearate the elasticity plots
  for (i in colnames(mm)) {
    plot_title <- paste0("Estimativa p/", i)

    table <- data.frame(
      v1 = seq(min(mm[[i]]), max(mm[[i]]), length.out = 200),
      v2 = seq(min(mmObs[[i]]), max(mmObs[[i]]), length.out = 200)
    )

    mmRes_aux <- mmRes

    mmRes_aux[[i]] <- table$v2

    preditos <- predict(
      object,
      newdata = mmRes_aux,
      interval = "confidence",
      level = 0.8
    )$PRED[, 1]

    table$y <- preditos

    plot <- ggplot2::ggplot(data = table, aes(x = v2, y = y)) +
      ggplot2::geom_line(col = "green4") +
      ggplot2::scale_y_continuous(
        labels = scales::number_format(big.mark = "."),
        n.breaks = 12
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::number_format(big.mark = "."),
        n.breaks = 3
      ) +
      ggplot2::labs(x = i, y = vari_dep, title = plot_title) +
      ggplot2::theme_bw()

    lista[[i]] <- plot
  }

  return(lista)
}
