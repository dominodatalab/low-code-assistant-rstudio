library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      "pre { background: white; }"
    )
  ),
  div(
    id = "data_selector",
    br(),
    radioButtons("datatype", NULL, inline = FALSE,
                 list("Upload a dataset" = "upload", "Data from your environment" = "existing")),
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
    wellPanel(
      div("Transformations", style = "font-size: 3rem; margin-bottom: 10px;"),
      fluidRow(
        column(12,
          actionButton("add_xform", "Add Transformation", class = "btn-success", style = "margin-right: 20px"),
          actionButton("undo", NULL, icon = icon("undo")),
          actionButton("redo", NULL, icon = icon("redo"))
        )
      )
    ),
    uiOutput("error"),
    div("Code:", style = "font-size: 3rem;"),
    verbatimTextOutput("code"),
    br(),
    reactable::reactableOutput("table")
  ))
)

server <- function(input, output, session) {
  dataname <- reactiveVal('mtcars')
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

  xform_modal <- TransformationModal$new("xform_modal")
  xform_modal_ret <- xform_modal$run()

  undo_redo <- UndoRedoStack$new(type = TransformationSequence$classname)

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
    initial_xforms <- TransformationSequence$new(name_in = dataname())
    xforms(initial_xforms)
    undo_redo$add(initial_xforms)
  })

  #--- New transformation
  observeEvent(input$add_xform, {
    xform_modal$show(data = xforms_result()$result, action = "add")
  })

  observeEvent(xform_modal_ret(), {
    new_xforms <- xforms()$add_transformation(xform_modal_ret())
    xforms(new_xforms)
    undo_redo$add(new_xforms)
  })

  error <- reactive({
    xforms_result()$error
  })

  observe({
    shinyjs::toggleState("add_xform", condition = is.null(error()))
  })

  output$error <- renderUI({
    req(error())
    div(class = "alert alert-danger", style="font-size:2rem", icon("exclamation-sign", lib = "glyphicon"), error())
  })

  output$table <- reactable::renderReactable({
    req(xforms_result()$result)
    reactable::reactable(
      xforms_result()$result,
      compact = TRUE,
      showPageSizeOptions = TRUE,
      defaultPageSize = 10,
      pageSizeOptions = c(10, 25, 50, 100),
      pagination = TRUE,
      highlight = TRUE,
      rownames = TRUE,
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
    xforms()$get_code()
  })

  #--- Undo/redo
  observeEvent(xforms(), {
    shinyjs::toggleState("undo", undo_redo$undo_size > 0)
    shinyjs::toggleState("redo", undo_redo$redo_size > 0)
  })

  observeEvent(input$undo, {
    new_xforms <- undo_redo$undo()$value
    xforms(new_xforms)
  })
  observeEvent(input$redo, {
    new_xforms <- undo_redo$redo()$value
    xforms(new_xforms)
  })
}

shinyApp(ui, server)

