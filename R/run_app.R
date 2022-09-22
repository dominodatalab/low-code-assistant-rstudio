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
run_app <- function() {
  shiny::shinyApp(app_ui, app_server, options = list(launch.browser = TRUE))
}
