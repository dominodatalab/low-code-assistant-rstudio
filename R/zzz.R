PACKAGE_NAME <- "assistDomino"

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(prefix = "lca-assets", directoryPath = system.file("assets", package = PACKAGE_NAME))
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
#' @import datasets
#' @import gapminder
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot
#' @importFrom haven read_sas
#' @importFrom readxl read_xls
#' @importFrom cli ansi_strip
#' @importFrom tools file_ext
#' @importFrom dplyr filter
#' @importFrom tidyr drop_na
#' @importFrom checkmate test_character
NULL
