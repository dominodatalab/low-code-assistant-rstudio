title_bar_ui <- function(id, title = NULL) {
  ns <- NS(id)

  div(
    html_dependency_lca(),
    class = "header-bar",
    class = "flex flex-gap1",
    if (is.null(title))
      textOutput(ns("page_title"))
    else
      title,
    div("Domino R Assistant", class = "flex-push"),
    img(height = "100%", src = "lca-assets/lca/img/domino-logo.svg")
  )
}

title_bar_server <- function(id, title = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      title_r <- make_reactive(title)
      output$page_title <- renderText(title_r())
    }
  )
}
