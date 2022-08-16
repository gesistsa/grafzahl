#' @method predict_model grafzahl
#' @export predict_model.grafzahl
predict_model.grafzahl <- function(x, newdata, type, ...) {
    if (!requireNamespace('grafzahl', quietly = TRUE)) {
        stop('grafzahl must be available when working with grafzahl models')
    }
    if (type == "raw") {
        res <- predict(x, newdata = newdata, return_raw = FALSE, ...)
        return(data.frame(Response = as.character(res), stringsAsFactors = FALSE))
    } else if (type == "prob") {
        res <- predict(x, newdata = newdata, return_raw = TRUE, ...)
        ey <- exp(res)
        output <- as.data.frame(ey / apply(ey, 1, sum))
        colnames(output) <- model$levels
        return(output)
    } else {
        stop("Unknown `type`.")
    }
}

#' @method model_type grafzahl
#' @export model_type.grafzahl
model_type.grafzahl <- function(x, ...) {
    if (x$regression) {
        return("regression")
    } else {
        return("classification")
    }
}
