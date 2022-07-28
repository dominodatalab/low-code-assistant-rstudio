# lca_UI <- function(id) {
#   ns <- NS(id)
#   tagList(
#
#   )
# }
#
# lca <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       dataset <- reactiveVal()
#
#
#     }
#   )
# }
#
#
# library(shiny)
#
# datasel <- DatasetSelector$new("popop")
#
# ui <- fluidPage(
#   #lca_UI("app")
#   datasel$ui()
# )
#
# server <- function(input, output, session) {
#   #lca("app")
#   gg<-datasel$server("tet")
#   observe(message(gg$num()))
# }
#
# shinyApp(ui, server)


library(shiny)

ui <- fluidPage(
  conditionalPanel(
    "input.show_table",
    reactable::reactableOutput("main_data")
  ),
  br(),
  fluidRow(
    column(4, actionButton("add_transformation", "+ Add Transformation")),
    column(2, checkboxInput("show_table", "Show table", value = TRUE)),
    column(2, checkboxInput("show_code", "Show code", value = FALSE))
  ),
  conditionalPanel(
    "input.show_code",
    verbatimTextOutput("code")
  )
)

server <- function(input, output, session) {
  main_data <- reactive({
    df <- mtcars
    row.names(df) <- seq(nrow(df))
    df[4, 5] <- NA
    df
  })
  output$main_data <- reactable::renderReactable({
    reactable::reactable(
      main_data(),
      showPageSizeOptions = TRUE,
      defaultPageSize = 10,
      pageSizeOptions = c(10, 25, 50, 100),
      pagination = TRUE,
      highlight = TRUE,
      rownames = TRUE,
      defaultColDef = reactable::colDef(
        align = "left"
      ),
      columns = list(
        .rownames = reactable::colDef(
          name = "#",
          width = 50,
          style = list(`font-style` = "italic"),
          headerStyle = list(`font-style` = "italic")
        )
      )
    )
  })

  output$code <- renderText({
    "df"
  })
}

shinyApp(ui, server)
