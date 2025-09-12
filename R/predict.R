#'@export
predict.estatelm <- function(
    object,
    newdata,
    interval = c("confidence", "prediction"),
    ...
) {
    transf <- NULL
    interval <- match.arg(interval)
    transf <- object$transf
    att <- attributes(object$terms)
    labels <- att$variables
    response_name <- as.character(labels[[2]])
    mode <- object$mode
    if (!is.null(transf)) {
        transformar_df <- function(df, transformacoes) {
            # Verificação de nomes válidos
            colunas <- names(transformacoes)
            # Aplica as transformações
            for (col in colunas) {
                funcao <- match.fun(transformacoes[[col]])
                df[[col]] <- funcao(df[[col]])
            }

            return(df)
        }

        newdata <- transformar_df(
            newdata,
            transf[names(newdata)]
        )
    }

    pred <- stats::predict.lm(
        object,
        newdata = newdata,
        interval = interval,
        ...
    )

    if (!is.null(transf)) {
        invStr <- transf[response_name]

        if (invStr == "I" | is.null(invStr)) {
            invFun <- I
        } else if (invStr == "log") {
            invFun <- exp
        } else if (invStr == "exp") {
            invFun <- log
        } else if (invStr == "log10") {
            invFun <- function(x) 10^x
        } else if (invStr == "log2") {
            invFun <- function(x) 2^x
        } else if (invStr == "sqrt") {
            invFun <- function(x) x^2
        } else if (invStr == "abs") {
            stop("A função 'abs' não é invertível para negativos.")
        } else if (invStr == "x^2" || invStr == "quadrado") {
            invFun <- sqrt # atenção: só para x >= 0
        } else if (invStr == "1/x" || invStr == "inverso") {
            invFun <- function(x) 1 / x
        } else if (invStr == "sin") {
            invFun <- asin
        } else if (invStr == "cos") {
            invFun <- acos
        } else if (invStr == "tan") {
            invFun <- atan
        } else if (invStr == "asin") {
            invFun <- sin
        } else if (invStr == "acos") {
            invFun <- cos
        } else if (invStr == "atan") {
            invFun <- tan
        } else if (invStr == "scale") {
            invFun <- function(x) {
                # Essa inversa só funciona se x ainda tiver atributos "scaled:center" e "scaled:scale"
                if (
                    is.null(attr(x, "scaled:center")) ||
                        is.null(attr(x, "scaled:scale"))
                ) {
                    stop(
                        "Inversa de scale requer os atributos 'scaled:center' e 'scaled:scale'."
                    )
                }
                x * attr(x, "scaled:scale") + attr(x, "scaled:center")
            }
        } else {
            stop(paste("Inversa não definida para a transformação:", invStr))
        }
        pred <- apply(pred, MARGIN = c(1, 2), FUN = invFun)
    }

    lista_dfs <- list()
    for (i in 1:nrow(pred)) {
        pred_aux <- pred[i, ]

        amp <- c(
            (pred_aux[2] - pred_aux[1]) / pred_aux[1],
            0,
            (pred_aux[3] - pred_aux[1]) / pred_aux[1]
        )

        ampSum <- sum(abs(amp))

        if (mode == "rural") {
            if (ampSum <= 0.3) {
                gp <- "III"
            } else if (ampSum <= 0.5) {
                gp <- "II"
            } else {
                gp <- "I"
            }
        } else {
            if (ampSum <= 0.3) {
                gp <- "III"
            } else if (ampSum <= 0.4) {
                gp <- "II"
            } else if (ampSum <= 0.5) {
                gp <- "I"
            } else {
                gp <- "Inadequado"
            }
        }

        gp <- c("-", gp, "-")

        df <- data.frame(
            est = c("Valor Mínimo", "Valor Médio", "Valor Máximo"),
            med = pred_aux[c(2, 1, 3)],
            amp = scales::percent(amp),
            gp = gp
        )

        colnames(df) <- c(
            "Estimativa",
            "Média",
            "Amplitude",
            "Grau de Precisão"
        )

        rownames(df) <- NULL

        lista_dfs[[paste0("df_", i)]] <- df
    }
    lista_dfs[["PRED"]] <- pred
    return(lista_dfs)
}
