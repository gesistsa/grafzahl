.initialize_conda <- function(envname, verbose = FALSE) {
    if (is.null(getOption('python_init'))) {
        reticulate::use_miniconda(envname, required = TRUE)
        options('python_init' = TRUE)
        if (verbose) {
            print(paste0("Conda environment ", envname, " is initialized.\n"))
        }
    }
}

## Create a factor-like thing that is zero-indexed. It should work with numeric vectors, char vectors and factors.
## The output is a vector 
.make_0i <- function(x) {
    levels <- unique(x)
    matching_levels <- seq(0, (length(levels) - 1))
    res <- matching_levels[match(x, levels)]
    attr(res, "levels") <- levels
    return(res)
}

.restore_0i <- function(x) {
    attr(x, "levels")[x + 1]
}

#' @export
grafzahl <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                     regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                     train_size = 0.8, args = NULL, cleanup = TRUE,
                     manual_seed = floor(runif(1, min = 1, max = 721831))) {
    UseMethod("grafzahl")
}

#' @export
grafzahl.default <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                             regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                             train_size = 0.8, args = NULL, cleanup = TRUE,
                             manual_seed = floor(runif(1, min = 1, max = 721831))) {
    return(NA)
}

#' @export
grafzahl.corpus <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                            regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                            train_size = 0.8, args = NULL, cleanup = TRUE,
                            manual_seed = floor(runif(1, min = 1, max = 721831))) {
    if (!is.integer(manual_seed)) {
        manual_seed <- as.integer(manual_seed)
    }
    .initialize_conda(.gen_envname(cuda = cuda))
    if (is.null(y)) {
        if (ncol(quanteda::docvars(x)) == 1) {
            y <- as.vector(quanteda::docvars(x)[,1])
        } else {
            stop("Please either specify `y` or set exactly one `docvars` in `x`.")
        }
    }
    if (!regression) {
        y <- .make_0i(y)
        num_labels <- length(attr(y, "levels"))
        levels <- attr(y, "levels")
    } else {
        levels <- NULL
    }
    input_data <- data.frame("text" = as.vector(x), "label" = as.vector(y))
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    output_dir <- normalizePath(output_dir)
    best_model_dir <- file.path(output_dir, "best_model")
    cache_dir <- normalizePath(tempdir())
    py_train(data = input_data, num_labels = num_labels, output_dir = output_dir, best_model_dir = best_model_dir, cache_dir = cache_dir, model_type = model_type, model_name = model_name, num_train_epochs = num_train_epochs, train_size = train_size, manual_seed = manual_seed)
    result <- list(
        call = match.call(),
        input_data = input_data,
        output_dir = output_dir,
        model_type = model_type,
        model_name = model_name,
        regression = regression,
        levels = levels,
        manual_seed = manual_seed
    )
    class(result) <- c("grafzahl", "textmodel_transformer", "textmodel", "list")
    if (cleanup & dir.exists(file.path("./", "runs"))) {
        unlink(file.path("./", "runs"), recursive = TRUE, force = TRUE)
    }
    return(result)
}

#' @export
textmodel_transformer <- function(...) {
    grafzahl(...)
}

#' @export
suggest_model <- function(x) {
    return(NA)
}

#' @export
predict.grafzahl <- function(object, newdata, cuda = detect_cuda(), return_raw = FALSE, ...) {
    .initialize_conda(.gen_envname(cuda = cuda))
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    res <- py_predict(to_predict = newdata, model_type = object$model_type, output_dir = object$output_dir, return_raw = return_raw)
    if (return_raw | is.null(object$levels)) {
        return(res)
    }
    return(object$levels[res + 1])
}

#' @export
print.grafzahl <- function(object, ...) {
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
        regression = regression,
        levels = NULL,
        manual_seed = NULL
    )
    class(result) <- c("grafzahl", "textmodel_transformer", "textmodel", "list")
    return(result)
}
