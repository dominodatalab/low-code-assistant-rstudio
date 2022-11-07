page_xforms_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinymixpanel::mp_init(
      token = MIXPANEL_TOKEN,
      userid = get_user_id(),
      options = MIXPANEL_CONFIG,
      default_properties = list(
        Domino_version = get_domino_version(),
        LCA_version = as.character(utils::packageVersion(PACKAGE_NAME)),
        LCA_language = "R"
      ),
      default_properties_js = list("domain" = "location.host"),
      test_token = MIXPANEL_TEST_TOKEN,
      test_domains = MIXPANEL_TEST_DOMAINS,
      track_server = TRUE
    ),
    shinyjs::useShinyjs(),
    html_dependency_lca(),

    title_bar_ui(ns("title"), "Transformations"),

    br(),
    shinyjs::hidden(
      div(
        id = ns("data_select"),
        data_environment_ui(ns("data_select_mod"))
      )
    ),

    shinyjs::hidden(
      div(
        id = ns("main_section"),

        div(
          class = "page-main-content",
          conditionalPanel(
            "input.show_table", ns = ns,
            xforms_table_ui(ns("table")),
            br()
          ),
          fluidRow(
            column(
              12,
              actionButton(ns("undo"), NULL, icon = icon("undo", verify_fa = FALSE)),
              actionButton(ns("redo"), NULL, icon = icon("redo", verify_fa = FALSE)),
              actionButton(ns("add_xform"), " ADD TRANSFORMATION", icon = icon("plus"), class = "btn-primary", style = "margin: 0 20px"),
              span(
                shinyWidgets::prettyCheckbox(
                  ns("show_table"), "Show Data", value = TRUE, width = "auto", shape = "curve", status = "primary", inline = TRUE
                ),
                shinyWidgets::prettyCheckbox(
                  ns("show_code"), "Show Code", value = TRUE, width = "auto", shape = "curve", status = "primary", inline = TRUE
                )
              )
            )
          ),
          uiOutput(ns("error")),
          conditionalPanel(
            "input.show_code", ns = ns,
            code_chunk_ui(ns("code"))
          )
        ),
        div(
          class = "page-actions flex flex-gap2",
          actionButton(
            ns("close"),
            "Close",
            icon = icon("close"),
            class = "btn-lg"
          ),
          actionButton(
            ns("continue"),
            "Apply",
            icon = icon("check"),
            class = "btn-primary btn-lg"
          )
        )
      )
    )
  )
}

page_xforms_server <- function(id, data_name_in = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      shinymixpanel::mp_track(
        MIXPANEL_EVENT_INIT,
        list(
          section = MIXPANEL_SECTION_XFORM
        )
      )

      observeEvent(input$close, {
        kill_app()
      })

      #--- Dealing with the input dataset

      if (is.null(data_name_in)) {
        shinyjs::show("data_select")

        data_select_mod <- data_environment_server("data_select_mod")

        observeEvent(data_select_mod$name(), {
          shinyjs::hide("data_select")
          shinyjs::show("main_section")
        })
        data_name <- reactive({
          data_select_mod$name()
        })
      } else {
        data_name_in_r <- make_reactive(data_name_in)
        shinyjs::show("main_section")

        data_name <- reactive({
          req(data_name_in_r())
          data_name_in_r()
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
      xforms_chunks <- reactive({
        xforms()$get_code_chunks()
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

      xform_modal <- transformation_modal("xform_modal")

      table <- xforms_table_server("table", result)

      code_section <- code_chunk_server(
        "code",
        chunks = xforms_chunks,
        error_line = error_line_num,
        editable = reactive(seq_len(xforms()$size)),
        skip = reactive(length(xforms()$dependencies))
      )

      undo_redo <- UndoRedoStack$new(type = TransformationSequence$classname)

      #--- New transformation
      observeEvent(input$add_xform, {
        xform_modal$show(data = result(), action = "add")
      })

      observeEvent(xform_modal$result(), {
        if (xform_modal$action() == "add") {
          shinymixpanel::mp_track(
            MIXPANEL_EVENT_INTERACTION,
            list(
              section = MIXPANEL_SECTION_XFORM,
              type = "add",
              transformation_type = class(xform_modal$result())[1]
            )
          )

          new_xforms <- xforms()$add(xform_modal$result())
        } else if (xform_modal$action() == "insert") {
          shinymixpanel::mp_track(
            MIXPANEL_EVENT_INTERACTION,
            list(
              section = MIXPANEL_SECTION_XFORM,
              type = "insert",
              transformation_type = class(xform_modal$result())[1]
            )
          )

          new_xforms <- xforms()$insert(xform_modal$result(), xform_modal$meta() - 1)
        } else if (xform_modal$action() == "edit") {
          shinymixpanel::mp_track(
            MIXPANEL_EVENT_INTERACTION,
            list(
              section = MIXPANEL_SECTION_XFORM,
              type = "edit",
              transformation_type = class(xform_modal$result())[1]
            )
          )

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
        div(class = "alert alert-danger", icon("exclamation-sign", lib = "glyphicon"), error())
      })

      #--- Undo/redo
      observeEvent(xforms(), {
        shinyjs::toggleState("undo", undo_redo$undo_size > 0)
        shinyjs::toggleState("redo", undo_redo$redo_size > 0)
      })

      observeEvent(input$undo, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_XFORM,
            type = "undo"
          )
        )

        new_xforms <- undo_redo$undo()$value
        xforms(new_xforms)
      })
      observeEvent(input$redo, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_XFORM,
            type = "redo"
          )
        )

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
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_XFORM,
            type = "delete",
            transformation_type = class(xforms()$transformations[[code_section$delete()]])[1]
          )
        )

        new_xforms <- xforms()$remove(code_section$delete())
        xforms(new_xforms)
        undo_redo$add(new_xforms)
      })

      #--- Actions were taken in the table
      observeEvent(table$drop(), {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_XFORM,
            type = "table_drop"
          )
        )

        new_xforms <- xforms()$add(table$drop())
        xforms(new_xforms)
        undo_redo$add(new_xforms)
      })
      observeEvent(table$missing(), {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_XFORM,
            type = "table_missing"
          )
        )

        xform_modal$show(data = result(), action = "add", xform = table$missing())
      })
      observeEvent(table$filter(), {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_XFORM,
            type = "table_filter"
          )
        )

        xform_modal$show(data = result(), action = "add", xform = table$filter())
      })

      observe({
        req(xforms())
        shinyjs::toggleState("continue", condition = is.null(error()))
      })

      observeEvent(input$continue, {
        if (xforms()$size > 0) {
          insert_text(paste0(xforms()$get_code()))
        }

        shinymixpanel::mp_track(
          MIXPANEL_EVENT_CODE,
          list(
            section = MIXPANEL_SECTION_XFORM
          )
        )

        kill_app()
      })
    }
  )
}
