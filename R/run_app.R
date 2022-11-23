#' Low Code Assistant - Data selection
#'
#' Run the data selection LCA wizard.
#' @export
assist_data <- function() {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(page_data_select_ui("app")),
    server = function(input, output, session) page_data_select_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' Low Code Assistant - Data selection
#'
#' Run the data selection LCA wizard.
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
    ui = shiny::fluidPage(page_xforms_ui("app")),
    server = function(input, output, session) page_xforms_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' Low Code Assistant - Transformations
#'
#' Run the data transformation LCA wizard.
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
    ui = shiny::fluidPage(page_viz_ui("app")),
    server = function(input, output, session) page_viz_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' Low Code Assistant - Visualizations
#'
#' Run the data visualizations LCA wizard.
#' @export
assist_viz_addin <- function() {
  assist_viz()
}

#' Low Code Assistant - Snippets
#'
#' Run the snippets LCA wizard.
#' @export
assist_snippets <- function() {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(page_snippets_ui("app")),
    server = function(input, output, session) page_snippets_server("app")
  )
  shiny::runGadget(app, viewer = shiny::dialogViewer("Domino R Assistant", 1100, 800))
}

#' Low Code Assistant - Snippets
#'
#' Run the snippets LCA wizard.
#' @export
assist_snippets_addin <- function() {
  assist_snippets()
}
