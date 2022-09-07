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
  function(data) {
    if (missing(data)) {
      rv()
    } else {
      attr(rv, ".impl")$private$dependents$invalidate()
      rv(data)
    }
  }
}

inelineUI <- function(tag) {
  htmltools::tagAppendAttributes(tag, style = "display: inline-block")
}
