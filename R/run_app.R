app_ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    html_dependency_lca(),
    lca_main_ui("lca")
  )
}

app_server <- function(input, output, session) {
  lca_main_server("lca")
}

#' @export
assist <- function() {
  shiny::shinyApp(app_ui, app_server, options = list(launch.browser = TRUE))
}

#' @export
run_gadget <- function() {
  shiny::runGadget(app_ui, app_server, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' @export
assist_data <- function() {
  app <- shinyApp(
    ui = fluidPage(page_data_select_ui("app", standalone = TRUE)),
    server = function(input, output, session) page_data_select_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' @export
assist_transform <- function() {
  app <- shinyApp(
    ui = fluidPage(page_xforms_ui("app", standalone = TRUE)),
    server = function(input, output, session) page_xforms_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' @export
assist_viz <- function() {
  app <- shinyApp(
    ui = fluidPage(page_viz_ui("app", standalone = TRUE)),
    server = function(input, output, session) page_viz_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}
