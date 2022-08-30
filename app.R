library(shiny)

ui <- fluidPage(
  lca_main_ui("lca")
)

server <- function(input, output, session) {
  lca_main_server("lca")
}

shinyApp(ui, server)
