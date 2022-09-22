html_dependency_lca <- function() {
  htmltools::htmlDependency(
    name = "lca",
    version = packageVersion("dominolca"),
    src = c(href = "lca-assets/lca"),
    package = "dominolca",
    script = c("js/shiny-utils.js"),
    stylesheet = c("css/lca.css")
  )
}

reactive_trigger <- function() {
  rv <- shiny::reactiveVal(0)
  list(
    depend = function() {
      invisible(rv())
    },
    trigger = function() {
      rv(isolate(rv() + 1))
    }
  )
}

reactiveValEvent <- function(data = NULL) {
  rv <- shiny::reactiveVal(data)
  rv_trigger <- shiny::reactiveVal(FALSE)
  function(data) {
    if (missing(data)) {
      rv_trigger() # Take a reactive dependency on rv_trigger
      rv()
    } else {
      rv_trigger(isolate(!rv_trigger())) # This should always result in rv_trigger invalidation
      rv(data)
    }
  }
}

inelineUI <- function(tag) {
  htmltools::tagAppendAttributes(tag, style = "display: inline-block")
}

kill_app <- function(session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("lca-close-window", TRUE)
  shiny::stopApp()
}
