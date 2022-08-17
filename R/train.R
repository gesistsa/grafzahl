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

#' Finetune a pretrained Transformer model for texts
#'
#' Fine tune (or train) a pretrained Transformer model for your given training labelled data `x` and `y`. The prediction task can be classification (if `regression` is `FALSE`, default) or regression (if `regression` is `TRUE`).
#' @param x the [corpus] or character vector of texts on which the model will be trained. Depending on `train_size`, some texts will be used for cross-validation.
#' @param y training labels. It can either be a single string indicating which [docvars] of the [corpus] is the training labels; a vector of training labels in either character or factor; or `NULL` if the [corpus] contains exactly one column in [docvars] and that column is the training labels. If `x` is a character vector, `y` must be a vector of the same length.
#' @param model_type a string indicating model_type of the input model. It can only be one of the following: "albert", "bert", "bertweet", "bigbird", "camembert", "deberta", "distilbert", "electra", "flaubert", "herbert", "layoutlm", "layoutlmv2", "longformer", "mpnet", "mobilebert", "rembert", "roberta", "squeezebert", "squeezebert", "xlm", "xlmroberta", "xlnet". This will be lowercased and hyphens will be removed, e.g. "XLM-RoBERTa" will be normalized to "xlmroberta". Please find this information in the `config.json` file of the model.
#' @param model_name string indicates either 1) the model name on Hugging Face website; 2) the local path of the model
#' @param regression logical, if `TRUE`, the task is regression, classification otherwise.
#' @param output_dir string, location of the output model. Important: Please note that if this directory exists, it will be overwritten.
#' @param cuda logical, whether to use CUDA, default to [detect_cuda()].
#' @param num_train_epochs numeric, if `train_size` is not exactly 1.0, the maximum number of epochs to try in the "early stop" regime will be this number times 5. If `train_size` is exactly 1.0, the number of epochs is exactly that.
#' @param train_size numeric, proportion of data in `x` and `y` to be used actually for training. The rest will be used for cross validation.
#' @param args list, additionally parameters to be used in the underlying simple transformers
#' @param cleanup logical, if `TRUE`, the `runs` directory generated will be removed when the training is done
#' @param manual_seed numeric, random seed
#' @param verbose logical, if `TRUE`, debug messages will be displayed
#' @param ... paramters pass to [grafzahl()]
#' @return a `grafzahl` S3 object
#' @examples
#' \dontrun{
#' library(quanteda)
#' library(quanteda.textmodels) ## for the data only
#' set.seed(20190721)
#' model <- grafzahl(x = data_corpus_moviereviews, y = "sentiment",
#'                  model_type = "bert", model_name = "bert-base-uncased",
#'                  train_size = 1, num_train_epochs = 2)
#' preds <- predict(model)
#' table(preds, docvars(data_corpus_moviereviews, "sentiment"))
#' }
#' @seealso [predict.grafzahl()]
#' @export
grafzahl <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                     regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                     train_size = 0.8, args = NULL, cleanup = TRUE,
                     manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
    UseMethod("grafzahl")
}

#' @rdname grafzahl
#' @export
grafzahl.default <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                             regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                             train_size = 0.8, args = NULL, cleanup = TRUE,
                             manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
    return(NA)
}

#' @rdname grafzahl
#' @export
grafzahl.corpus <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                            regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                            train_size = 0.8, args = NULL, cleanup = TRUE,
                            manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
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
    if (quanteda::ndoc(x) <= 1) {
        stop("Too few documents.")
    }
    if (length(y) == 1) {
        ## It should be a docvars name, but it's better check
        if (!y %in% colnames(quanteda::docvars(x))) {
            stop(paste0(y, " is not a docvar."))
        }
        y <- as.vector(quanteda::docvars(x)[,y])
    }
    if (!regression) {
        y <- .make_0i(y)
        num_labels <- length(attr(y, "levels"))
        levels <- attr(y, "levels")
    } else {
        num_labels <- 1L
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
    py_train(data = input_data, num_labels = num_labels, output_dir = output_dir, best_model_dir = best_model_dir, cache_dir = cache_dir, model_type = model_type, model_name = model_name, num_train_epochs = num_train_epochs, train_size = train_size, manual_seed = manual_seed, regression = regression, verbose = verbose)
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

#' @rdname grafzahl
#' @export
textmodel_transformer <- function(...) {
    grafzahl(...)
}

#' @rdname grafzahl
#' @export
grafzahl.character <- function(x, y = NULL, model_type = "xlmroberta", model_name = "xlm-roberta-base",
                            regression = FALSE, output_dir = "./output", cuda = detect_cuda(), num_train_epochs = 4,
                            train_size = 0.8, args = NULL, cleanup = TRUE,
                            manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
    if (is.null(y)) {
        stop("`y` cannot be NULL when x is a character vector.", call. = FALSE)
    }
    if (length(x) != length(y)) {
        stop("`y` must have the same length as `x`.", call. = FALSE)
    }
    grafzahl(x = quanteda::corpus(x), y = y, model_type = model_type, model_name = model_name, regression = regression,
             output_dir = output_dir, cuda = cuda, num_train_epochs = num_train_epochs, train_size = train_size,
             args = args, cleanup = cleanup, manual_seed = manual_seed, verbose = verbose)
}

## #' @export
## suggest_model <- function(x) {
##     return(NA)
## }

#' Prediction from a fine-tuned grafzahl object
#'
#' Make prediction from a fine-tuned grafzahl object.
#' @param object an S3 object trained with [grafzahl()]
#' @param newdata a [corpus] or a character vector of texts on which prediction should be made
#' @inheritParams grafzahl
#' @param return_raw logical, if `TRUE`, return a matrix of logits; a vector of class prediction otherwise
#' @param ... not used
#' @return a vector of class prediction or a matrix of logits
#' @method predict grafzahl
#' @export
predict.grafzahl <- function(object, newdata, cuda = detect_cuda(), return_raw = FALSE, ...) {
    if (missing(newdata)) {
        if (!is.data.frame(object$input_data)) {
            stop("`newdata` is missing. And no input data in the `grafzahl` object.", call. = FALSE)
        }
        newdata <- object$input_data$text
    }
    .initialize_conda(.gen_envname(cuda = cuda))
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    res <- py_predict(to_predict = newdata, model_type = object$model_type, output_dir = object$output_dir, return_raw = return_raw)
    if (return_raw | is.null(object$levels)) {
        return(res)
    }
    return(object$levels[res + 1])
}

#' @method print grafzahl
#' @export
print.grafzahl <- function(x, ...) {
    return(NA)
}

#' Detect cuda
#'
#' This function detects whether cuda is available. If `setup_grafzahl` was executed with `cuda` being `FALSE`, this function will return `FALSE`. Even if `setup_grafzahl` was executed with `cuda` being `TRUE` but with any factor that can't enable cuda (e.g. no Nvidia GPU, the environment was incorrectly created), this function will also return `FALSE`.
#' @return boolean, whether cuda is available.
#' @export
detect_cuda <- function() {
    allenvs <- reticulate::conda_list()$name
    .initialize_conda(grep("^grafzahl_condaenv", allenvs, value = TRUE)[1])
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    return(py_detect_cuda())
}

#' Create a grafzahl S3 object from the output_dir
#'
#' Create a grafzahl S3 object from the output_dir
#' @inheritParams grafzahl
#' 
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
