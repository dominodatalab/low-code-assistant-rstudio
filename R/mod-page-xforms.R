page_xforms_ui <- function(id) {
  ns <- NS(id)

  shinyjs::hidden(
    div(
      id = ns("transformation_section"),
      br(),
      wellPanel(
        fluidRow(
          column(
            12,
            actionButton(ns("undo"), NULL, icon = icon("undo")),
            actionButton(ns("redo"), NULL, icon = icon("redo")),
            actionButton(ns("add_xform"), "Add Transformation", class = "btn-success", style = "margin: 0 20px"),
            inelineUI(checkboxInput(ns("show_code"), "Show code", TRUE, width = 110)),
            inelineUI(checkboxInput(ns("show_table"), "Show data", TRUE))
          )
        ),
        br(), br(), tags$strong("Choose transformation to edit/delete/insert before"),
        fluidRow(
          column(2, selectInput(ns("xform_idx"), NULL, "")),
          actionButton(ns("edit"),"edit"),
          actionButton(ns("insert"),"insert"),
          actionButton(ns("delete"),"delete")
        )
      ),
      uiOutput(ns("error")),
      conditionalPanel(
        "input.show_code", ns = ns,
        div("Code:", style = "font-size: 3rem;"),
        verbatimTextOutput(ns("code")),
        br()
      ),
      conditionalPanel(
        "input.show_table", ns = ns,
        xforms_table_ui(ns("table"))
      )
    )
  )
}

page_xforms_server <- function(id, data_name) {
  moduleServer(
    id,
    function(input, output, session) {

      #--- Hide/show this module

      # The show/hide code can't be placed inside a function and is instead
      # in an observer because of https://github.com/rstudio/shiny/issues/2706
      show_trigger <- reactive_trigger()
      hide_trigger <- reactive_trigger()

      observe({
        req(show_trigger$depend() > 0)
        isolate({
          shinyjs::show("transformation_section")
        })
      })

      observe({
        req(hide_trigger$depend() > 0)
        isolate({
          shinyjs::hide("transformation_section")
          shinyjs::reset("transformation_section")
        })
      })

      #--- Dealing with the dataset and TransformationsSequence

      main_data <- reactive({
        req(data_name())
        get(data_name(), envir = .GlobalEnv)
      })

      observeEvent(data_name(), {
        initial_xforms <- TransformationSequence$new(name_in = data_name())
        xforms(initial_xforms)
        undo_redo$add(initial_xforms)
      })

      xforms <- reactiveVal()
      xforms_result <- reactive({
        req(xforms())
        isolate({
          assign(data_name(), main_data(), envir = .GlobalEnv)
          xforms()$run(env = .GlobalEnv)
        })
      })
      error <- reactive({
        xforms_result()$error
      })
      result <- reactive({
        xforms_result()$result
      })

      xform_modal <- transformation_modal("xform_modal")

      table <- xforms_table_server("table", result)

      undo_redo <- UndoRedoStack$new(type = TransformationSequence$classname)

      #--- New transformation
      observeEvent(input$add_xform, {
        xform_modal$show(data = result(), action = "add")
      })

      observeEvent(xform_modal$result(), {
        if (xform_modal$action() == "add") {
          new_xforms <- xforms()$add(xform_modal$result())
        } else if (xform_modal$action() == "insert") {
          new_xforms <- xforms()$insert(xform_modal$result(), xform_modal$meta() - 1)
        } else if (xform_modal$action() == "edit") {
          new_xforms <- xforms()$transformations
          new_xforms[[xform_modal$meta()]] <- xform_modal$result()
          new_xforms <- TransformationSequence$new(new_xforms, name_in = xforms()$name_in)
        }
        xforms(new_xforms)
        undo_redo$add(new_xforms)
      })

      observe({
        shinyjs::toggleState("add_xform", condition = is.null(error()))
      })

      output$error <- renderUI({
        req(error())
        div(class = "alert alert-danger", style="font-size:2rem", icon("exclamation-sign", lib = "glyphicon"), error())
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

      # edit/modify/delete
      observeEvent(xforms(), {
        updateSelectInput(session, "xform_idx", choices = seq_len(xforms()$size))
      })
      xform_to_modify <- reactive({
        as.numeric(input$xform_idx)
      })

      observeEvent(input$edit, {
        temp_xform <- xforms()$head(xform_to_modify() - 1)
        new_env <- new.env()
        assign(data_name(), main_data(), envir = new_env)
        temp_res <- temp_xform$run(new_env)$result
        xform_modal$show(data = temp_res, action = "edit", xform = xforms()$transformations[[xform_to_modify()]], meta = xform_to_modify())
      })

      observeEvent(input$insert, {
        temp_xform <- xforms()$head(xform_to_modify() - 1)
        new_env <- new.env()
        assign(data_name(), main_data(), envir = new_env)
        temp_res <- temp_xform$run(new_env)$result
        xform_modal$show(data = temp_res, action = "insert", meta = xform_to_modify())
      })

      observeEvent(input$delete, {
        new_xforms <- xforms()$remove(xform_to_modify())
        xforms(new_xforms)
        undo_redo$add(new_xforms)
      })

      #--- Actions were taken in the table
      observeEvent(table$drop(), {
        new_xforms <- xforms()$add(table$drop())
        xforms(new_xforms)
        undo_redo$add(new_xforms)
      })
      observeEvent(table$missing(), {
        xform_modal$show(data = result(), action = "add", xform = table$missing())
      })
      observeEvent(table$filter(), {
        xform_modal$show(data = result(), action = "add", xform = table$filter())
      })

      return(list(
        show = function() show_trigger$trigger(),
        hide = function() hide_trigger$trigger()
      ))

    }
  )
}
