PACKAGE_NAME <- "assistDomino"

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(prefix = "lca-assets", directoryPath = system.file("assets", package = PACKAGE_NAME))

  # Tell {reticulate} to use the Conda version of Python available on Domino Data Lab.
  #
  # Provide other options in case the package is being used for local development.
  #
  PYTHON_PATH <- c(
    "/opt/conda/bin/python",
    "/usr/bin/python"
  )
  #
  for (path in PYTHON_PATH) {
    if (file.exists(path)) {
      message("Using Python interpreter at ", path, ".")
      reticulate::use_python(path)
      break
    }
  }
  #
  # If no Python is present (or not in the expected place), then install MiniConda.
  #
  if (!reticulate::py_available(initialize = TRUE)) {
    try(reticulate::install_miniconda())
  }

  # Install the (Python) domino_data package.
  #
  if (!reticulate::py_module_available("domino_data")) {
    reticulate::py_install("dominodatalab-data", pip = TRUE, method = "virtualenv")
  }
}

.onUnload <- function(libname, pkgname) {
  if (utils::packageVersion("shiny") >= "1.4.0") {
    shiny::removeResourcePath("lca-assets")
  }
}

# Workaround for https://github.com/rstudio/rstudio/issues/12078
#' @import miniUI
NULL

# Import shiny because the code is too messy to include shiny:: everywhere
#' @import shiny
NULL

# Empty imports to get rid of CRAN check warnings
#' @import DominoDataR
#' @import datasets
#' @import gapminder
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot
#' @importFrom haven read_sas
#' @importFrom readxl read_xls
#' @importFrom cli ansi_strip
#' @importFrom tools file_ext
NULL
