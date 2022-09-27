page_xforms_ui <- function(id, standalone = TRUE) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    html_dependency_lca(),
    if (standalone) title_bar_ui(ns("title"), "Transformations"),
    shinyjs::hidden(checkboxInput(ns("standalone"), NULL, standalone)),
    shinyjs::hidden(
      div(
        id = ns("data_select"),
        br(),
        data_environment_ui(ns("data_select_mod"))
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("xforms_main_section"),
        conditionalPanel(
          "input.show_table", ns = ns,
          xforms_table_ui(ns("table")),
          br()
        ),
        fluidRow(
          column(
            12,
            actionButton(ns("undo"), NULL, icon = icon("undo")),
            actionButton(ns("redo"), NULL, icon = icon("redo")),
            actionButton(ns("add_xform"), " ADD TRANSFORMATION", icon = icon("plus"), class = "btn-primary", style = "margin: 0 20px"),
            inelineUI(checkboxInput(ns("show_code"), "Show code", TRUE, width = "auto")),
            inelineUI(checkboxInput(ns("show_table"), "Show data", TRUE))
          )
        ),
        uiOutput(ns("error")),
        conditionalPanel(
          "input.show_code", ns = ns,
          xforms_code_chunk_ui(ns("code"))
        ),
        br(),
        div(
          class = "no-margin flex flex-gap2",
          actionButton(ns("close"), "Close"),
          div(class = "flex-push"),
          hide_if_standalone(
            standalone,
            shinyWidgets::prettyCheckbox(
              ns("insert_code"),
              "Insert Code",
              value = TRUE,
              width = "auto",
              shape = "curve",
              status = "primary"
            )
          ),
          actionButton(
            ns("continue"),
            if (standalone) "Apply" else "Continue",
            icon = if (standalone) icon("check") else icon("angle-double-right"),
            class = "btn-primary btn-lg"
          )
        )
      )
    ),
    br()
  )
}

page_xforms_server <- function(id, data_name_in = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      result_rv <- reactiveValues(name = NULL, data = NULL)

      observeEvent(input$close, {
        kill_app()
      })

      #--- Dealing with the input dataset

      if (is.null(data_name_in)) {
        shinyjs::show("data_select")

        data_select_mod <- data_environment_server("data_select_mod")
        data_name <- reactive({
          req(data_select_mod$name())
          shinyjs::hide("data_select")
          shinyjs::show("xforms_main_section")
          data_select_mod$name()
        })
      } else {
        shinyjs::show("xforms_main_section")

        data_name <- reactive({
          req(data_name_in())
          data_name_in()
        })
      }

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
      error_line_num <- reactive({
        xforms_result()$error_line_num
      })
      result <- reactive({
        xforms_result()$result
      })
      name_out <- reactive({
        req(xforms())
        tail(unlist(xforms()$get_code_chunks()), 1)
      })

      xform_modal <- transformation_modal("xform_modal")

      table <- xforms_table_server("table", result)

      code_section <- xforms_code_chunk_server("code", chunks = reactive(xforms()$get_code_chunks()), error_line = error_line_num)

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
      observeEvent(code_section$modify(), {
        temp_xform <- xforms()$head(code_section$modify() - 1)
        new_env <- new.env()
        assign(data_name(), main_data(), envir = new_env)
        temp_res <- temp_xform$run(new_env)$result
        xform_modal$show(data = temp_res, action = "edit", xform = xforms()$transformations[[code_section$modify()]], meta = code_section$modify())
      })

      observeEvent(code_section$insert(), {
        temp_xform <- xforms()$head(code_section$insert() - 1)
        new_env <- new.env()
        assign(data_name(), main_data(), envir = new_env)
        temp_res <- temp_xform$run(new_env)$result
        xform_modal$show(data = temp_res, action = "insert", meta = code_section$insert())
      })

      observeEvent(code_section$delete(), {
        new_xforms <- xforms()$remove(code_section$delete())
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

      observe({
        req(xforms())
        shinyjs::toggleState("continue", condition = is.null(error()))
        shinyjs::toggleState("insert_code", condition = (is.null(error()) && xforms()$size > 0))
      })

      observeEvent(input$continue, {
        if (input$insert_code) {
          if (xforms()$size > 0) {
            insert_text(paste0(xforms()$get_code(), "\n"))
          }
        }

        result_rv$name <- name_out()
        result_rv$data <- result()

        if (input$standalone) {
          kill_app()
        }
      })

      return(list(
        done = reactive(input$continue),
        name = reactive(result_rv$name),
        data = reactive(result_rv$data)
      ))

    }
  )
}
