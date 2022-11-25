.have_conda <- function() {
    ## !is.null(tryCatch(reticulate::conda_list(), error = function(e) NULL))
    ## Not a very robust test, but take it.
    dir.exists(reticulate::miniconda_path())
}

.gen_envname <- function(cuda = TRUE) {
    envname <- "grafzahl_condaenv"
    if (cuda) {
        envname <- paste0(envname, "_cuda")
    }
    return(envname)
}


.install_gpu_pytorch <- function(cuda_version) {
    .initialize_conda(.gen_envname(cuda = TRUE))
    conda_path <- file.path(reticulate::miniconda_path(), "bin/conda")
    system2(conda_path, args = c("install", "-n", .gen_envname(cuda = TRUE), "pytorch", "pytorch-cuda", paste0("cudatoolkit=", cuda_version), "-c", "pytorch", "-c", "nvidia", "-y"))
    python_path <- reticulate::py_config()$python
    system2(python_path, args = c("-m", "pip", "install", "simpletransformers"))
}

#' Setup grafzahl
#'
#' Install a self-contained miniconda environment with all Python components (PyTorch, Transformers, Simpletransformers, etc) which grafzahl required. The default location is "~/.local/share/r-miniconda/envs/grafzahl_condaenv" (suffix "_cuda" is added if `cuda` is `TRUE`).
#' On Linux or Mac and if miniconda is not found, this function will also install miniconda.
#' @param cuda logical, if `TRUE`, indicate whether a CUDA-enabled environment is wanted.
#' @param force logical, if `TRUE`, delete previous environment (if exists) and create a new environment
#' @param cuda_version character, indicate CUDA version, ignore if `cuda` is `FALSE`
#' @examples
#' # setup an environment with cuda enabled.
#' \dontrun{
#' setup_grafzahl(cuda = TRUE)
#' }
#' @export
setup_grafzahl <- function(cuda = FALSE, force = FALSE, cuda_version = "11.3") {
    envname <- .gen_envname(cuda = cuda)
    if (!.have_conda()) {
        if (!force) {
            message("No conda was found in this system.")
            ans <- utils::menu(c("No", "Yes"), title = paste0("Do you want to install miniconda in ", reticulate::miniconda_path()))
            if (ans == 1) {
                stop("Setup aborted.\n")
            } else {
                reticulate::install_miniconda()
            }
        } else {
            reticulate::install_miniconda()
        }
    }
    if (envname %in% reticulate::conda_list()$name & !force) {
        stop(paste0("Conda environment ", envname, " already exists.\nForce reinstallation by setting `force` to `TRUE`.\n"))
    }
    if (envname %in% reticulate::conda_list()$name & force) {
        reticulate::conda_remove(envname)
    }
    
    ## The actual installation
    ## https://github.com/rstudio/reticulate/issues/779
    conda_path <- file.path(reticulate::miniconda_path(), "bin/conda")
    if (cuda) {
        yml_file <- "grafzahl_gpu.yml"
    } else {
        yml_file <- "grafzahl.yml"
    }
    system2(conda_path, args = c("env", "create",  paste0("-f=", system.file(yml_file, package = 'grafzahl')), "-n", envname, "python=3.9"))
    if (cuda) {
        .install_gpu_pytorch(cuda_version = cuda_version)
    }
}
