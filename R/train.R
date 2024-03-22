.say <- function(verbose, ...) {
    if (isTRUE(verbose)) {
        message(paste(..., sep = " "))
    }
    invisible()
}

.download_from_huggingface <- function(model_name, json_file = tempfile()) {
    json_url <- paste0("https://huggingface.co/", model_name, "/raw/main/config.json")
    tryCatch({
        suppressWarnings(download.file(url = json_url, destfile = json_file, quiet = TRUE))
    }, error = function(e) {
        stop("Fail to download the model `", model_name, "` from Hugging Face", call. = FALSE)
    })
    return(json_file)
}

.infer_model_type <- function(model_name) {
    if (!dir.exists(model_name)) {
        json_file <- .download_from_huggingface(model_name)
    } else {
        json_file <- file.path(model_name, "config.json")
    }
    jsonlite::fromJSON(json_file)$model_type
}

.check_model_type <- function(model_type, model_name) {
    if (missing(model_name)) {
        stop("You must provide `model_name`", call. = FALSE)
    }
    if (is.null(model_type)) {
        model_type <- .infer_model_type(model_name)
    }
    model_type <- gsub("-", "", tolower(model_type))
    if (!model_type %in% grafzahl::supported_model_types) {
        stop("Invalid `model_type`.", call. = FALSE)
    }
    return(model_type)    
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

.prepare_y <- function(y, x) {
    if (is.null(y)) {
        if (ncol(quanteda::docvars(x)) == 1) {
            return(as.vector(quanteda::docvars(x)[,1]))
        } else {
            stop("Please either specify `y` or set exactly one `docvars` in `x`.", call. = FALSE)
        }
    }
    if (length(y) == 1) {
        ## It should be a docvars name, but it's better check
        if (!y %in% colnames(quanteda::docvars(x))) {
            stop(paste0(y, " is not a docvar."))
        }
        return(as.vector(quanteda::docvars(x)[,y]))
    }
    return(y)
}

##stole from quanteda
.generate_meta <- function() {
    list("package-version" = utils::packageVersion("grafzahl"),
         "r-version" = getRversion(),
         "system" = Sys.info()[c("sysname", "machine", "user")],
         "directory" = getwd(),
         "created" = Sys.Date())
}

.generate_random_dir <- function(lowest = 1, highest = 1000) {
    random_dir <- file.path(tempdir(), sample(seq(from = lowest, to = highest), 1))
    if (!dir.exists(random_dir)) {
        dir.create(random_dir)
    }
    return(normalizePath(random_dir))
}

.create_object <- function(call = NA, input_data = NULL, output_dir, model_type, model_name = NA, regression, levels = NULL, manual_seed = NULL) {
    result <- list(
        call = call,
        input_data = input_data,
        output_dir = output_dir,
        model_type = model_type,
        model_name = model_name,
        regression = regression,
        levels = levels,
        manual_seed = manual_seed,
        meta = .generate_meta()
    )
    class(result) <- c("grafzahl", "textmodel_transformer", "textmodel", "list")
    return(result)
}

#' Fine tune a pretrained Transformer model for texts
#'
#' Fine tune (or train) a pretrained Transformer model for your given training labelled data `x` and `y`. The prediction task can be classification (if `regression` is `FALSE`, default) or regression (if `regression` is `TRUE`).
#' @param x the [corpus] or character vector of texts on which the model will be trained. Depending on `train_size`, some texts will be used for cross-validation.
#' @param y training labels. It can either be a single string indicating which [docvars] of the [corpus] is the training labels; a vector of training labels in either character or factor; or `NULL` if the [corpus] contains exactly one column in [docvars] and that column is the training labels. If `x` is a character vector, `y` must be a vector of the same length.
#' @param model_name string indicates either 1) the model name on Hugging Face website; 2) the local path of the model
#' @param regression logical, if `TRUE`, the task is regression, classification otherwise.
#' @param output_dir string, location of the output model. If missing, the model will be stored in a temporary directory. Important: Please note that if this directory exists, it will be overwritten.
#' @param cuda logical, whether to use CUDA, default to [detect_cuda()].
#' @param num_train_epochs numeric, if `train_size` is not exactly 1.0, the maximum number of epochs to try in the "early stop" regime will be this number times 5 (i.e. 4 * 5 = 20 by default). If `train_size` is exactly 1.0, the number of epochs is exactly that.
#' @param train_size numeric, proportion of data in `x` and `y` to be used actually for training. The rest will be used for cross validation.
#' @param args list, additionally parameters to be used in the underlying simple transformers
#' @param cleanup logical, if `TRUE`, the `runs` directory generated will be removed when the training is done
#' @param model_type a string indicating model_type of the input model. If `NULL`, it will be inferred from `model_name`. Supported model types are available in [supported_model_types].
#' @param manual_seed numeric, random seed
#' @param verbose logical, if `TRUE`, debug messages will be displayed
#' @param ... paramters pass to [grafzahl()]
#' @return a `grafzahl` S3 object with the following items
#' \item{call}{original function call} 
#' \item{input_data}{input_data for the underlying python function}
#' \item{output_dir}{location of the output model}
#' \item{model_type}{model type}
#' \item{model_name}{model name}
#' \item{regression}{whether or not it is a regression model}
#' \item{levels}{factor levels of y}
#' \item{manual_seed}{random seed}
#' \item{meta}{metadata about the current session}
#' @examples
#' if (detect_conda() && interactive()) {
#' library(quanteda)
#' set.seed(20190721)
#' ## Using the default cross validation method
#' model1 <- grafzahl(unciviltweets, model_type = "bertweet", model_name = "vinai/bertweet-base")
#' predict(model1)
#'
#' ## Using LIME
#' input <- corpus(ecosent, text_field = "headline")
#' training_corpus <- corpus_subset(input, !gold)
#' model2 <- grafzahl(x = training_corpus,
#'                  y = "value",
#'                  model_name = "GroNLP/bert-base-dutch-cased")
#' test_corpus <- corpus_subset(input, gold)
#' predicted_sentiment <- predict(model2, test_corpus)
#' require(lime)
#' sentences <- c("Dijsselbloem pessimistisch over snelle stappen Grieken",
#'                "Aandelenbeurzen zetten koersopmars voort")
#' explainer <- lime(training_corpus, model2)
#' explanations <- explain(sentences, explainer, n_labels = 1,
#'                         n_features = 2)
#' plot_text_explanations(explanations)
#' }
#' @seealso [predict.grafzahl()]
#' @export
grafzahl <- function(x, y = NULL, model_name = "xlm-roberta-base",
                     regression = FALSE, output_dir, cuda = detect_cuda(), num_train_epochs = 4,
                     train_size = 0.8, args = NULL, cleanup = TRUE, model_type = NULL,
                     manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
    UseMethod("grafzahl")
}

#' @rdname grafzahl
#' @export
grafzahl.default <- function(x, y = NULL, model_name = "xlm-roberta-base",
                             regression = FALSE, output_dir, cuda = detect_cuda(), num_train_epochs = 4,
                             train_size = 0.8, args = NULL, cleanup = TRUE, model_type = NULL,
                             manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
    return(invisible(NULL))
}

#' @rdname grafzahl
#' @export
grafzahl.corpus <- function(x, y = NULL, model_name = "xlm-roberta-base",
                            regression = FALSE, output_dir, cuda = detect_cuda(), num_train_epochs = 4,
                            train_size = 0.8, args = NULL, cleanup = TRUE, model_type = NULL,
                            manual_seed = floor(runif(1, min = 1, max = 721831)), verbose = TRUE) {
    if (quanteda::ndoc(x) <= 1) {
        stop("Too few documents.")
    }
    y <- .prepare_y(y, x)
    if (!regression) {
        y <- .make_0i(y)
        num_labels <- length(attr(y, "levels"))
        levels <- attr(y, "levels")
    } else {
        num_labels <- 1L
        levels <- NULL
    }
    if (!is.integer(manual_seed)) {
        manual_seed <- as.integer(manual_seed)
    }
    model_type <- .check_model_type(model_type = model_type, model_name = model_name)
    input_data <- data.frame("text" = as.vector(x), "label" = as.vector(y))
    if (Sys.getenv("KILL_SWITCH") == "KILL") {
        return(NA)
    }
    if (missing(output_dir)) {
        output_dir <- .generate_random_dir()
        .say(verbose, "No `output_dir` provided. The output model will be written to:", output_dir, "\n")      
    } else {
        if (dir.exists(output_dir)) {
            .say(verbose, output_dir, "exists. Will be overwritten.\n")
        } else {
            dir.create(output_dir)
            .say(verbose, output_dir, "created.\n")
        }
        output_dir <- normalizePath(output_dir)
    }
    best_model_dir <- file.path(output_dir, "best_model")
    cache_dir <- .generate_random_dir(9999, 300000)
    .initialize_python(envname = .gen_envname(cuda = cuda), verbose = verbose)
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    if (isTRUE(getOption("grafzahl.nonconda"))) {
        .say(verbose, "[Non-conda MODE] If you are running this on Google Colab, you will not see the training progress.")
    }
    py_train(data = input_data, num_labels = num_labels, output_dir = output_dir, best_model_dir = best_model_dir, cache_dir = cache_dir, model_type = model_type, model_name = model_name, num_train_epochs = num_train_epochs, train_size = train_size, manual_seed = manual_seed, regression = regression, verbose = verbose)
    if (cleanup && dir.exists(file.path("./", "runs"))) {
        unlink(file.path("./", "runs"), recursive = TRUE, force = TRUE)
    }
    result <- .create_object(call = match.call(),
                             input_data = input_data,
                             output_dir = output_dir,
                             model_type = model_type,
                             model_name = model_name,
                             regression = regression,
                             levels = levels,
                             manual_seed = manual_seed)
    return(result)
}

#' @rdname grafzahl
#' @export
textmodel_transformer <- function(...) {
    grafzahl(...)
}

#' @rdname grafzahl
#' @export
grafzahl.character <- function(x, y = NULL, model_name = "xlmroberta",
                            regression = FALSE, output_dir, cuda = detect_cuda(), num_train_epochs = 4,
                            train_size = 0.8, args = NULL, cleanup = TRUE, model_type = NULL,
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


#' Prediction from a fine-tuned grafzahl object
#'
#' Make prediction from a fine-tuned grafzahl object.
#' @param object an S3 object trained with [grafzahl()]
#' @param newdata a [corpus] or a character vector of texts on which prediction should be made.
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
    if (Sys.getenv("KILL_SWITCH") == "KILL") {
        return(NA)
    }
    .initialize_python(envname = .gen_envname(cuda = cuda), verbose = FALSE)
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    res <- py_predict(to_predict = newdata, model_type = object$model_type, output_dir = object$output_dir, return_raw = return_raw, use_cuda = cuda)
    if (return_raw || is.null(object$levels)) {
        return(res)
    }
    return(object$levels[res + 1])
}

#' @method print grafzahl
#' @export
print.grafzahl <- function(x, ...) {
    if (is.data.frame(x$input_data)) {
        n_training <- nrow(x$input_data)
    }
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "output_dir:", x$output_dir, ";",
        "model_type:", x$model_type, ";",
        "model_name:", x$model_name, ";",
        n_training, "training documents; ",
        "\n", sep = " ")
}

#' Create a grafzahl S3 object from the output_dir
#'
#' Create a grafzahl S3 object from the output_dir
#' @inherit grafzahl return params
#' @export
hydrate <- function(output_dir, model_type = NULL, regression = FALSE) {
    if (missing(output_dir)) {
        stop("You must provide `output_dir`")
    }
    model_type <- .check_model_type(model_type = model_type, model_name = output_dir)
    results <- .create_object(
        output_dir = output_dir,
        model_type = model_type,
        regression = regression,
    )
    return(results)
}
