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

#' Low Code Assistant - Data selection
#'
#' Run the data selection LCA wizard.
#' @export
assist_data <- function() {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(page_data_select_ui("app", standalone = TRUE)),
    server = function(input, output, session) page_data_select_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' @export
assist_data_addin <- function() {
  assist_data()
}

#' Low Code Assistant - Transformations
#'
#' Run the data transformation LCA wizard.
#' @export
assist_transform <- function() {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(page_xforms_ui("app", standalone = TRUE)),
    server = function(input, output, session) page_xforms_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' @export
assist_transform_addin <- function() {
  assist_transform()
}

#' Low Code Assistant - Visualizations
#'
#' Run the data visualizations LCA wizard.
#' @export
assist_viz <- function() {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(page_viz_ui("app", standalone = TRUE)),
    server = function(input, output, session) page_viz_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' @export
assist_viz_addin <- function() {
  assist_viz()
}
