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
textmodel_transformer <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base", regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4, train_size = 0.8, args = NULL, cleanup = TRUE) {
    UseMethod("textmodel_transformer")
}

#' @export
textmodel_transformer.default <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base", regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4, train_size = 0.8, args = NULL, cleanup = TRUE) {
    return(NA)
}

## assume that eval_prop = 0, no eval_df, no early
## 

#' @export
textmodel_transformer.corpus <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base", regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4, train_size = 0.8, args = NULL, cleanup = TRUE) {
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
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    output_dir <- normalizePath(output_dir)
    best_model_dir <- file.path(output_dir, "best_model")
    cache_dir <- normalizePath(tempdir())
    py_train(data = input_data, num_labels = num_labels, output_dir = output_dir, best_model_dir = best_model_dir, cache_dir = cache_dir, model_type = model_type, model_name = model_name, num_train_epochs = num_train_epochs, train_size = train_size)
    result <- list(
        call = match.call(),
        input_data = input_data,
        output_dir = output_dir,
        model_type = model_type,
        model_name = model_name,
        regression = regression
    )
    class(result) <- c("textmodel_transformer", "textmodel", "list")
    if (cleanup & dir.exists(file.path("./", "runs"))) {
        unlink(file.path("./", "runs"), recursive = TRUE, force = TRUE)
    }
    return(result)
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
predict.textmodel_transformer <- function(object, newdata, cuda = detect_cuda(), return_raw = FALSE, ...) {
    .initialize_conda(.gen_envname(cuda = cuda))
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    return(py_predict(to_predict = newdata, model_type = object$model_type, output_dir = object$output_dir, return_raw = return_raw))
}

#' @export
print.textmodel_transformer <- function(object, ...) {
    return(NA)
}

#' @export
detect_cuda <- function() {
    allenvs <- reticulate::conda_list()$name
    .initialize_conda(grep("^grafzahl_condaenv", allenvs, value = TRUE)[1])
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    return(py_detect_cuda())
}

#' @export
hydrate <- function(output_dir, model_type, regression = FALSE) {
    if (missing(model_type) & missing(output_dir)) {
        stop("You must provide both `output_dir` and `model_type`")
    }
    result <- list(
        call = NA,
        input_data = NA,
        output_dir = output_dir,
        model_type = model_type,
        model_name = NA,
        regression = regression
    )
    class(result) <- c("textmodel_transformer", "textmodel", "list")
    return(result)
}
