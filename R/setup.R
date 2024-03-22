.is_windows <- function() {
    Sys.info()[['sysname']] == "Windows"
}

.gen_conda_path <- function(envvar = "GRAFZAHL_MINICONDA_PATH", bin = FALSE) {
    if (Sys.getenv(envvar) == "") {
        main_path <- reticulate::miniconda_path()
    } else {
        main_path <- Sys.getenv(envvar)
    }
    if (isFALSE(bin)) {
        return(main_path)
    }
    if (.is_windows()) {
        return(file.path(main_path, "Scripts", "conda.exe"))
    }
    file.path(main_path, "bin", "conda")
}

## list all conda envs, but restrict to .gen_conda_path
## Should err somehow
.list_condaenvs <- function() {
    all_condaenvs <- reticulate::conda_list(conda = .gen_conda_path(bin = TRUE))
    ##if (.is_windows()) {
    return(all_condaenvs$name)
    ##}
    ##all_condaenvs[grepl(.gen_conda_path(), all_condaenvs$python),]$name
}

.have_conda <- function() {
    ## !is.null(tryCatch(reticulate::conda_list(), error = function(e) NULL))
    ## Not a very robust test, but take it.
    file.exists(.gen_conda_path(bin = TRUE))
}

#' @rdname detect_cuda
#' @export
detect_conda <- function() {
    if(!.have_conda()) {
        return(FALSE)
    }
    envnames <- grep("^grafzahl_condaenv", .list_condaenvs(), value = TRUE)
    length(envnames) != 0
}

.gen_envname <- function(cuda = TRUE) {
    envname <- "grafzahl_condaenv"
    if (cuda) {
        envname <- paste0(envname, "_cuda")
    }
    return(envname)
}

.initialize_python <- function(envname, verbose = FALSE) {
    if (is.null(getOption("python_init")) && isTRUE(getOption("grafzahl.nonconda"))) {
        options("python_init" = TRUE)
        .say(verbose = verbose, "[Non-conda MODE] Use a non-conda Python environment. The environment is not checked.")
        return(invisible(NULL))
    }
    if (is.null(getOption("python_init"))) {
        if (.is_windows()) {
            python_executable <- file.path(.gen_conda_path(), "envs", envname, "python.exe")
        } else {
            python_executable <- file.path(.gen_conda_path(), "envs", envname, "bin", "python")
        }
        ## Until rstydio/reticulate#1308 is fixed; mask it for now
        Sys.setenv(RETICULATE_MINICONDA_PATH = .gen_conda_path())
        reticulate::use_miniconda(python_executable, required = TRUE)
        options("python_init" = TRUE)
        .say(verbose = verbose, "Conda environment ", envname, " is initialized.")
    }
    return(invisible(NULL))
}

#' Detecting Miniconda And Cuda
#'
#' These functions detects miniconda and cuda.
#'
#' `detect_conda` conducts a test to check whether 1) a miniconda installation and 2) the grafzahl miniconda environment exist.
#' 
#' `detect_cuda` checks whether cuda is available. If `setup_grafzahl` was executed with `cuda` being `FALSE`, this function will return `FALSE`. Even if `setup_grafzahl` was executed with `cuda` being `TRUE` but with any factor that can't enable cuda (e.g. no Nvidia GPU, the environment was incorrectly created), this function will also return `FALSE`.
#' @return boolean, whether the system is available.
#' @export
detect_cuda <- function() {
    options("python_init" = NULL)
    if (Sys.getenv("KILL_SWITCH") == "KILL") {
        return(NA)
    }
    if (is.null(getOption("grafzahl.nonconda"))) {
        envnames <- grep("^grafzahl_condaenv", .list_condaenvs(), value = TRUE)
        if (length(envnames) == 0) {
            stop("No conda environment found. Run `setup_grafzahl` to bootstrap one.")
        }
        if ("grafzahl_condaenv_cuda" %in% envnames) {
            envname <- "grafzahl_condaenv_cuda"
        } else {
            envname <- "grafzahl_condaenv"
        }
    } else {
        envname <- NA
    }
    .initialize_python(envname = envname, verbose = FALSE)
    reticulate::source_python(system.file("python", "st.py", package = "grafzahl"))
    return(py_detect_cuda())
}

.install_gpu_pytorch <- function(cuda_version) {
    .initialize_python(.gen_envname(cuda = TRUE))
    conda_executable <- .gen_conda_path(bin = TRUE)
    status <- system2(conda_executable, args = c("install", "-n", .gen_envname(cuda = TRUE), "pytorch", "pytorch-cuda", paste0("cudatoolkit=", cuda_version), "-c", "pytorch", "-c", "nvidia", "-y"))
    if (status != 0) {
        stop("Cannot set up `pytorch`.")
    }    
    python_executable <- reticulate::py_config()$python
    status <- system2(python_executable, args = c("-m", "pip", "install", "simpletransformers", "\"transformers\"", "\"scipy\""))
    if (status != 0) {
        stop("Cannot set up `simpletransformers`.")
    }    
}

#' Setup grafzahl
#'
#' Install a self-contained miniconda environment with all Python components (PyTorch, Transformers, Simpletransformers, etc) which grafzahl required. The default location is "~/.local/share/r-miniconda/envs/grafzahl_condaenv" (suffix "_cuda" is added if `cuda` is `TRUE`).
#' On Linux or Mac and if miniconda is not found, this function will also install miniconda. The path can be changed by the environment variable `GRAFZAHL_MINICONDA_PATH`
#' @param cuda logical, if `TRUE`, indicate whether a CUDA-enabled environment is wanted.
#' @param force logical, if `TRUE`, delete previous environment (if exists) and create a new environment
#' @param cuda_version character, indicate CUDA version, ignore if `cuda` is `FALSE`
#' @examples
#' # setup an environment with cuda enabled.
#' if (detect_conda() && interactive()) {
#'     setup_grafzahl(cuda = TRUE)
#' }
#' @return TRUE (invisibly) if installation is successful.
#' @export
setup_grafzahl <- function(cuda = FALSE, force = FALSE, cuda_version = "11.3") {
    envname <- .gen_envname(cuda = cuda)
    if (!.have_conda()) {
        if (!force) {
            message("No conda was found in ", .gen_conda_path())
            ans <- utils::menu(c("No", "Yes"), title = paste0("Do you want to install miniconda in ", .gen_conda_path()))
            if (ans == 1) {
                stop("Setup aborted.\n")
            }
        }
        reticulate::install_miniconda(.gen_conda_path(bin = FALSE), update = TRUE, force = TRUE)
    }
    allenvs <- .list_condaenvs()
    if (envname %in% allenvs && !force) {
        stop(paste0("Conda environment ", envname, " already exists.\nForce reinstallation by setting `force` to `TRUE`.\n"))
    }
    if (envname %in% allenvs && force) {
        reticulate::conda_remove(envname = envname, conda = .gen_conda_path(bin = TRUE))
    }    
    ## The actual installation
    ## https://github.com/rstudio/reticulate/issues/779
    ##conda_executable <- file.path(.gen_conda_path(), "bin/conda")
    if (isTRUE(cuda)) {
        yml_file <- "grafzahl_gpu.yml"
    } else {
        yml_file <- "grafzahl.yml"
    }
    status <- system2(.gen_conda_path(bin = TRUE), args = c("env", "create",  paste0("-f=", system.file(yml_file, package = 'grafzahl')), "-n", envname))
    if (status != 0) {
        stop("Cannot set up the basic conda environment.")
    }
    if (isTRUE(cuda)) {
        .install_gpu_pytorch(cuda_version = cuda_version)
    }
    ## Post-setup checks
    if (!detect_conda()) {
        stop("Conda can't be detected.")
    }
    if (detect_cuda() != cuda) {
        stop("Cuda wasn't configurated correctly.")
    }
    return(invisible(TRUE))
}

#' Set up grafzahl to be used on Google Colab or similar environments
#'
#' Set up grafzahl to be used on Google Colab or similar environments. This function is also useful if you do not
#' want to use conda on a local machine, e.g. you have configurateed the required Python package.
#'
#' @param install logical, whether to install the required Python packages
#' @param check logical, whether to perform a check after the setup. The check displays 1) whether CUDA can be detected, 2) whether
#' the non-conda mode has been activated, i.e. whether the option 'grafzahl.nonconda' is `TRUE`.
#' @param verbose, logical, whether to display messages
#' @examples
#' # A typical use case for Google Colab
#' if (interactive()) {
#'     use_nonconda()
#' }
#' @return TRUE (invisibly) if installation is successful.
#' @export
use_nonconda <- function(install = TRUE, check = TRUE, verbose = TRUE) {
    if (install) {
        system("python3 -m pip install simpletransformers emoji", intern = TRUE, ignore.stdout = !verbose)
    }
    options("grafzahl.nonconda" = TRUE)
    if (check) {
        .say(verbose, "Post-setup Check:")
        .say(verbose, "CUDA detected: ", detect_cuda())
        .say(verbose, "Non-conda mode activated: ", isTRUE(getOption("grafzahl.nonconda")))
    }
    return(invisible(TRUE))
}
