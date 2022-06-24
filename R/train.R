.initialize_conda <- function(envname, verbose = FALSE) {
    if (is.null(getOption('python_init'))) {
        reticulate::use_miniconda(envname, required = TRUE)
        options('python_init' = TRUE)
        if (verbose) {
            print(paste0("Conda environment ", envname, " is initialized.\n"))
        }
    }
}

#' @export
textmodel_transformer <- function(x, y, model_name = NULL, regression = FALSE, model_type = NULL, output_dir = "output",
                                  cuda = FALSE, args = NULL) {
    ## TO BE IMPLEMENTED
    UseMethod("textmodel_transformer")
}

#' @export
textmodel_transformer.default <- function(x, y, model_type = NULL, model_name = NULL, regression = FALSE, output_dir = "output",
                                          cuda = FALSE, args = NULL) {
    return(NA)
}

#' @export
textmodel_transformer.corpus <- function(x, y, model_type = NULL, model_name = NULL, regression = FALSE, output_dir = "output",
                                         cuda = FALSE, args = NULL) {
    .initialize_conda(.gen_envname(cuda = cuda))
    return(NA)
}

#' @export
suggest_model <- function(x) {
    return(NA)
}

#' @export
predict.textmodel_transformer <- function(object, newdata, cuda = FALSE, ...) {
    .initialize_conda(.gen_envname(cuda = cuda))
    return(NA)
}

#' @export
print.textmodel_transformer <- function(object, ...) {
    return(NA)
}

### There should be some form of hydration (just from the outputdir).
