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
textmodel_transformer <- function(x, y = NULL, model_name = NULL, regression = FALSE, model_type = NULL, output_dir = "output",
                                  cuda = detect_cuda(), args = NULL) {
    ## TO BE IMPLEMENTED
    UseMethod("textmodel_transformer")
}

#' @export
textmodel_transformer.default <- function(x, y = NULL, model_type = NULL, model_name = NULL, regression = FALSE, output_dir = "output",
                                          cuda = detect_cuda(), args = NULL) {
    return(NA)
}

#' @export
textmodel_transformer.corpus <- function(x, y = NULL, model_type = NULL, model_name = NULL, regression = FALSE, output_dir = "output",
                                         cuda = detect_cuda(), args = NULL) {
    .initialize_conda(.gen_envname(cuda = cuda))
    if (is.null(y)) {
        if (ncol(quanteda::docvars(x)) == 1) {
            y <- as.vector(quanteda::docvars(x)[,1])
        } else {
            stop("Please either specify `y` or set exactly one `docvars` in `x`.")
        }
    }
    ## if (!is.factor(y) & !regression) {
    ##     y <- as.factor(y)
    ## }
    input_data <- data.frame("text" = as.vector(x), "label" = y, stringsAsFactors = FALSE)
    if (!regression) {
        num_labels <- length(levels(as.factor(y)))
    }
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    py_train(input_data, num_labels)
    return(input_data)
}

#' @export
grafzahl <- function(...) {
    textmodel_transformer(...)
}

#' @export
suggest_model <- function(x) {
    return(NA)
}

#' @export
predict.textmodel_transformer <- function(object, newdata, cuda = detect_cuda(), ...) {
    .initialize_conda(.gen_envname(cuda = cuda))
    return(NA)
}

#' @export
print.textmodel_transformer <- function(object, ...) {
    return(NA)
}

### There should be some form of hydration (just from the outputdir).

#' @export
detect_cuda <- function() {
    allenvs <- reticulate::conda_list()$name
    .initialize_conda(grep("^grafzahl_condaenv", allenvs, value = TRUE)[1])
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    return(py_detect_cuda())
}
