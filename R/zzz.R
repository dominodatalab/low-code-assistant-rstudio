PACKAGE_NAME <- "assistDomino"

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(prefix = "lca-assets", directoryPath = system.file("assets", package = PACKAGE_NAME))
}

.onUnload <- function(libname, pkgname) {
  if (utils::packageVersion("shiny") >= "1.4.0") {
    shiny::removeResourcePath("lca-assets")
  }
}

# Empty imports to get rid of CRAN check warnings
#' @import R6
#' @import datasets
#' @import gapminder
#' @import ggplot2
#' @import haven
#' @import miniUI
#' @import readxl
NULL
