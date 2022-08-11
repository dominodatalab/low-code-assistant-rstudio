library(shiny)

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
        selectInput("action_type", NULL, c("Remove columns" = "drop",
                                      "Select columns" = "select",
                                      "Filter rows" = "filter"))
      ),
      column(
        4,
        actionButton("apply_xform", "Apply")
      )
    ),
    conditionalPanel(
      "input.action_type == 'drop'",
      fluidRow(
        column(4, selectInput("drop_cols", "Columns", NULL, multiple = TRUE)),
        column(4, textInput("drop_name", "New name", "df"))
      )
    ),
    conditionalPanel(
      "input.action_type == 'select'",
      fluidRow(
        column(4, selectInput("select_cols", "Columns", NULL, multiple = TRUE)),
        column(4, textInput("select_name", "New name", "df"))
      )
    ),
    conditionalPanel(
      "input.action_type == 'filter'",
      fluidRow(
        column(4, selectInput("filter_col", "Column", NULL)),
        column(2, selectInput("filter_op", "Operation", unname(FilterTransformation$OPTIONS))),
        column(2, textInput("filter_value", "Value", "")),
        column(4, textInput("filter_name", "New name", "df"))
      )
    )),
    fluidRow(
      column(2, actionButton("undo", "undo")),
      column(2, actionButton("redo", "redo"))
    ),
    DT::DTOutput("table"),
    verbatimTextOutput("availables")
  ))
)

server <- function(input, output, session) {
  dataname <- reactiveVal()
  main_data <- reactive({
    req(dataname())
    get(dataname(), envir = .GlobalEnv)
  })

  xforms <- reactiveVal()
  xforms_result <- reactive({
    req(xforms())
    isolate({
      assign(dataname(), main_data(), envir = .GlobalEnv)
      xforms()$run(env = .GlobalEnv)
    })
  })

  undo_stack <- reactiveVal(list())
  redo_stack <- reactiveVal(list())

  #--- dataset selection
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
  observeEvent(dataname(), {
    shinyjs::hide("data_selector")
    shinyjs::show("transformation_section")
    xforms(TransformationSequence$new(name_in = dataname()))
  })

  #--- update transformations dropdowns with the latest data columns
  observeEvent(xforms_result(), {
    updateSelectInput(session, "drop_cols", choices = names(xforms_result()$result))
    updateSelectInput(session, "select_cols", choices = names(xforms_result()$result))
    updateSelectInput(session, "filter_col", choices = names(xforms_result()$result))
  })

  #--- New transformation
  observeEvent(input$apply_xform, {
    if (input$action_type == "drop") {
      action <- DropTransformation$new(cols = input$drop_cols, name_out = input$drop_name)
    } else if (input$action_type == "select") {
      action <- SelectTransformation$new(cols = input$select_cols, name_out = input$select_name)
    } else if (input$action_type == "filter") {
      action <- FilterTransformation$new(col = input$filter_col, op = input$filter_op, value = input$filter_value, name_out = input$filter_name)
    }

    new_undo <- append(undo_stack(), xforms())
    undo_stack(new_undo)

    redo_stack(list())

    new_xforms <- xforms()$add_transformation(action)
    xforms(new_xforms)
  })

  error <- reactive({
    xforms_result()$error
  })

  output$error <- renderUI({
    req(error())
    div(class = "alert alert-danger", style="font-size:2rem", icon("exclamation-sign", lib = "glyphicon"), error())
  })

  output$table <- DT::renderDT({
    DT::datatable(
      xforms_result()$result
    )
  })

  output$code <- renderText({
    xforms()$get_code()
  })

  output$availables <- renderText({
    xforms_result()
    names(Filter(function(x) is(x, "data.frame"), mget(ls(envir = .GlobalEnv), envir = .GlobalEnv)))
  })

  #--- Undo/redo
  observeEvent(undo_stack(), {
    shinyjs::toggleState("undo", length(undo_stack()) > 0)
  })
  observeEvent(redo_stack(), {
    shinyjs::toggleState("redo", length(redo_stack()) > 0)
  })

  observeEvent(input$undo, {
    new_redo <- append(redo_stack(), xforms())
    redo_stack(new_redo)

    xforms(tail(undo_stack(), 1)[[1]])
    undo_stack(head(undo_stack(), -1))
  })
  observeEvent(input$redo, {
    new_undo <- append(undo_stack(), xforms())
    undo_stack(new_undo)

    xforms(tail(redo_stack(), 1)[[1]])
    redo_stack(head(redo_stack(), -1))
  })
}

shinyApp(ui, server)

