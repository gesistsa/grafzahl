.have_conda <- function() {
    !is.null(tryCatch(reticulate::conda_list(), error = function(e) NULL))
}

.gen_envname <- function(cuda = TRUE) {
    envname <- "grafzahl_condaenv"
    if (cuda) {
        envname <- paste0(envname, "_cuda")
    }
    return(envname)
}

#' @export
setup_grafzahl <- function(cuda = FALSE, force = FALSE) {
    envname <- .gen_envname(cuda = cuda)
    if (!.have_conda()) {
        cat("No conda was found in this system.")
        ans <- utils::menu(c("No", "Yes"), title = paste0("Do you want to install miniconda in ", reticulate::miniconda_path()))
        if (ans == 1) {
            stop("Setup aborted.\n")
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
    system2(conda_path, args = c("env", "create",  paste0("-f=", system.file("grafzahl.yml", package = 'grafzahl')), "-n", envname, "python=3.9"))
}
