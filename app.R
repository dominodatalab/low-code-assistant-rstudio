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

########

ui <- fluidPage(
  br(),
  verbatimTextOutput("code"),
  br(),
  wellPanel(fluidRow(
    column(
      4,
      selectInput("action", NULL, c("Remove columns" = "drop",
                                    "Select columns" = "select",
                                    "Filter rows" = "filter"))
    ),
    column(
      4,
      actionButton("doit", "Apply")
    )
  ),
  conditionalPanel(
    "input.action == 'drop'",
    fluidRow(
      column(4, selectInput("drop_cols", "Columns", names(mtcars), multiple = TRUE)),
      column(4, textInput("drop_name", "New name", "df"))
    )
  ),
  conditionalPanel(
    "input.action == 'select'",
    fluidRow(
      column(4, selectInput("select_cols", "Columns", names(mtcars), multiple = TRUE)),
      column(4, textInput("select_name", "New name", "df"))
    )
  ),
  conditionalPanel(
    "input.action == 'filter'",
    fluidRow(
      column(4, selectInput("filter_col", "Column", names(mtcars))),
      column(2, selectInput("filter_op", "Operation", unname(FilterTransformation$TRANSFORMATIONS))),
      column(2, textInput("filter_value", "Value", "")),
      column(4, textInput("filter_name", "New name", "df"))
    )
  )),
  DT::DTOutput("table"),
)

server <- function(input, output, session) {
  actions <- reactiveVal(TransformationSequence$new(name_in = "mtcars"))

  observeEvent(input$doit, {
    if (input$action == "drop") {
      action <- DropTransformation$new(cols = input$drop_cols, name_out = input$drop_name)
    } else if (input$action == "select") {
      action <- SelectTransformation$new(cols = input$select_cols, name_out = input$select_name)
    } else if (input$action == "filter") {
      action <- FilterTransformation$new(col = input$filter_col, op = input$filter_op, value = input$filter_value, name_out = input$filter_name)
    }
    old_trans <- actions()$get_transformations()
    new_trans <- append(old_trans, action)
    actions(TransformationSequence$new(new_trans, name_in = "mtcars"))
  })

  output$table <- DT::renderDT({
    DT::datatable(
      actions()$apply()
    )
  })

  output$code <- renderText({
    actions()$get_code()
  })
}

shinyApp(ui, server)
