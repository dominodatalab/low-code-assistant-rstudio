.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(prefix = "lca-assets", directoryPath = system.file("assets", package = "assistDomino"))
}

.onUnload <- function(libname, pkgname) {
  if (utils::packageVersion("shiny") >= "1.4.0") {
    shiny::removeResourcePath("lca-assets")
  }
}
