html_dependency_lca <- function() {
  htmltools::htmlDependency(
    name = "lca",
    version = utils::packageVersion(PACKAGE_NAME),
    src = c(href = "lca-assets/lca"),
    package = PACKAGE_NAME,
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

make_reactive <- function(x) {
  if (shiny::is.reactive(x)) {
    x
  } else {
    shiny::reactive(x)
  }
}

input_with_checkbox <- function(input, checkboxId, checkboxLabel, checkboxValue = FALSE, side = c("left", "right")) {
  stopifnot(inherits(input, "shiny.tag"))
  side <- match.arg(side)

  checkbox <- tags$label(
    class = "input-group-addon",
    tags$input(
      id = checkboxId,
      checked = if (isTRUE(checkboxValue)) "checked" else NULL,
      type = "checkbox", style = "vertical-align: middle"
    ),
    tags$span(checkboxLabel, style = "vertical-align: middle")
  )

  input_idx <- which(sapply(input$children, function(x) x$name == "input"))
  if (length(input_idx) != 1) {
    stop("Can't find the <input> tag")
  }

  input_tag <- input$children[[input_idx]]

  if (side == "left") {
    group <- tags$div(class = "input-group", checkbox, input_tag)
  } else if (side == "right") {
    group <- tags$div(class = "input-group", input_tag, checkbox)
  } else {
    stop("Invalid 'side'")
  }

  input$children[[input_idx]] <- group
  input
}
