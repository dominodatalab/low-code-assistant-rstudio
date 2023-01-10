page_data_select_ui <- function(id) {
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

    title_bar_ui(ns("title"), "Load Data"),

    div(
      class = "page-main-content",
      tabsetPanel(
        id = ns("import_modules"),

        tabPanel(
          "Upload",
          value = "upload",
          icon = icon("upload"),
          LoadComponentUpload$shiny$ui(ns("upload"))
        ),
        tabPanel(
          "URL",
          value = "url",
          icon = icon("link"),
          LoadComponentURL$shiny$ui(ns("url"))
        ),
        tabPanel(
          "Datasets",
          value = "dataset",
          icon = icon("table"),
          LoadComponentDatasets$shiny$ui(ns("datasets"))
        ),
        tabPanel(
          "Project Files",
          value = "file",
          icon = icon("folder-open"),
          LoadComponentProjectFile$shiny$ui(ns("project_files"))
        ),
        tabPanel(
          "Demo Data",
          value = "demo",
          icon = icon("file"),
          LoadComponentDemo$shiny$ui(ns("demo"))
        )
      ),
      tags$script(glue::glue("$('#{ns(\"import_modules\")}').addClass('nav-justified');")),
      uiOutput(ns("error")),
      div(
        shinyWidgets::prettyCheckbox(
          ns("show_preview"), "Show Preview", value = TRUE, width = "auto", shape = "curve", status = "primary", inline = TRUE
        ),
        shinyWidgets::prettyCheckbox(
          ns("show_code"), "Show Code", value = TRUE, width = "auto", shape = "curve", status = "primary", inline = TRUE
        )
      ),
      conditionalPanel(
        "input.show_preview", ns = ns,
        htmltools::tagAppendAttributes(
          tableOutput(ns("preview_data")),
          class = "no-margin small-table"
        ),
        br()
      ),
      conditionalPanel(
        "input.show_code", ns = ns,
        shinycodeviewer::code_viewer_ui(ns("code")),
        br()
      )
    ),
    div(
      class = "page-actions flex flex-gap2",
      input_with_checkbox(
        textInput(ns("varname"), "Output variable", "df"),
        checkboxId = ns("custom_name"),
        checkboxLabel = "Custom name",
        checkboxValue = TRUE
      ),
      actionButton(
        ns("close"),
        "Close",
        icon = icon("close"),
        class = "btn-lg"
      ),
      actionButton(
        ns("continue"),
        "Insert Code",
        icon = icon("check"),
        class = "btn-primary btn-lg"
      )
    )
  )
}

page_data_select_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      shinymixpanel::mp_track(
        MIXPANEL_EVENT_INIT,
        list(
          section = MIXPANEL_SECTION_LOAD
        )
      )

      observeEvent(input$import_modules, ignoreInit = TRUE, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_LOAD,
            type = "select tab",
            tab = input$import_modules
          )
        )
      })

      observeEvent(input$close, {
        kill_app()
      })

      data_modules <- list(
        upload = LoadComponentUpload$shiny$server("upload", upload_dir = get_user_upload_dir()),
        url = LoadComponentURL$shiny$server("url"),
        dataset = LoadComponentDatasets$shiny$server("datasets"),
        file = LoadComponentProjectFile$shiny$server("project_files"),
        demo = LoadComponentDemo$shiny$server("demo")
      )

      selected_data_module <- reactive({
        data_modules[[input$import_modules]]
      })
      name_in <- reactive({
        selected_data_module()$name()
      })
      code_in <- reactive({
        selected_data_module()$code()
      })
      data <- reactive({
        selected_data_module()$data()
      })
      error <- reactive({
        selected_data_module()$error()
      })

      observeEvent(input$varname, {
        if (!is_valid_name(input$varname)) {
          updateTextInput(session, "varname", value = make.names(input$varname))
        }
      })

      name_out <- reactive({
        req(name_in())
        if (input$custom_name) {
          make.names(input$varname)
        } else {
          make.names(name_in())
        }
      })

      observe({
        shinyjs::toggleState("varname", condition = input$custom_name)
        shinyjs::toggleCssClass("varname", "notext", condition = !input$custom_name)
      })

      output$error <- renderUI({
        req(error())
        div(class = "alert alert-danger", icon("exclamation-sign", lib = "glyphicon"), error())
      })

      code_out <- reactive({
        req(code_in(), name_out())
        paste0(name_out(), " <- ", code_in())
      })

      shinycodeviewer::code_viewer_server("code", code_out)

      output$preview_data <- renderTable({
        req(data())
        utils::head(data(), 5)
      }, striped = TRUE, bordered = TRUE, spacing = "xs")

      validated <- reactive({
        !is.null(data()) && nrow(data()) > 0 && is_valid_name(name_out())
      })

      observe({
        shinyjs::toggleState("continue", condition = validated())
      })

      observeEvent(input$continue, {
        assign_to_global(name_out(), data())

        if (code_in() != name_out()) {
          insert_text(code_out())
        }

        shinymixpanel::mp_track(
          MIXPANEL_EVENT_CODE,
          list(
            section = MIXPANEL_SECTION_LOAD,
            data_type = input$import_modules
          )
        )

        kill_app()
      })
    }
  )
}
