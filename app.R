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
  shinyjs::useShinyjs(),
  div(
    id = "data_selector",
    radioButtons("datatype", NULL, inline = FALSE,
                 list("Upload a dataset" = "upload", "Existing dataset" = "existing")),
    conditionalPanel(
      "input.datatype == 'upload'",
      fileInput("file", NULL)
    ),
    conditionalPanel(
      "input.datatype == 'existing'",
      selectInput("data", NULL, c("", names(Filter(function(x) is(x, "data.frame"), mget(envir = .GlobalEnv, ls(envir = .GlobalEnv))))))
    )
  ),
  shinyjs::hidden(div(
    id = "transformation_section",
    br(),
    uiOutput("error"),
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
        column(4, selectInput("drop_cols", "Columns", NULL, multiple = TRUE)),
        column(4, textInput("drop_name", "New name", "df"))
      )
    ),
    conditionalPanel(
      "input.action == 'select'",
      fluidRow(
        column(4, selectInput("select_cols", "Columns", NULL, multiple = TRUE)),
        column(4, textInput("select_name", "New name", "df"))
      )
    ),
    conditionalPanel(
      "input.action == 'filter'",
      fluidRow(
        column(4, selectInput("filter_col", "Column", NULL)),
        column(2, selectInput("filter_op", "Operation", unname(FilterTransformation$OPTIONS))),
        column(2, textInput("filter_value", "Value", "")),
        column(4, textInput("filter_name", "New name", "df"))
      )
    )),
    DT::DTOutput("table"),
    verbatimTextOutput("availables")
  ))
)

server <- function(input, output, session) {
  dataname <- reactiveVal("mtcars")
  undo_stack <- reactiveVal(list())
  redo_stack <- reactiveVal(list())

  main_data <- reactive({
    req(dataname())
    get(dataname(), envir = .GlobalEnv)
  })

  observeEvent(input$file, {
    data <- read.csv(input$file$datapath)
    name <- tools::file_path_sans_ext(input$file$name)
    assign(name, data, envir = .GlobalEnv)
    dataname(name)
  })
  observeEvent(input$data, {
    req(nzchar(input$data))
    dataname(input$data)
  })



  actions <- reactiveVal()
  observeEvent(dataname(), {
    shinyjs::hide("data_selector")
    shinyjs::show("transformation_section")
    actions(TransformationSequence$new(name_in = dataname())$run())
  })
  observeEvent(main_data(), {
    updateSelectInput(session, "drop_cols", choices = names(main_data()))
    updateSelectInput(session, "select_cols", choices = names(main_data()))
    updateSelectInput(session, "filter_col", choices = names(main_data()))
  })


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
    actions(TransformationSequence$new(new_trans, name_in = dataname())$run(.GlobalEnv))

    old_undo <- undo_stack()
    new_undo <- append(old_undo(), old_trans)
    undo_stack(new_undo)
  })

  error <- reactive({
    actions()$get_error()
  })

  output$error <- renderUI({
    req(error())
    div(class = "alert alert-danger", style="font-size:2rem", icon("exclamation-sign", lib = "glyphicon"), error())
  })

  output$table <- DT::renderDT({
    DT::datatable(
      actions()$get_result()
    )
  })

  output$code <- renderText({
    actions()$get_code()
  })

  output$availables <- renderText({
    actions()
    names(Filter(function(x) is(x, "data.frame"), mget(ls(envir = .GlobalEnv), envir = .GlobalEnv)))
  })
}

shinyApp(ui, server)
