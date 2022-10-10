page_data_select_ui <- function(id, standalone = TRUE) {
  ns <- NS(id)

  tagList(
    mp_init(
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
      test_domains = MIXPANEL_TEST_DOMAINS
    ),
    shinyjs::useShinyjs(),
    html_dependency_lca(),
    if (standalone) title_bar_ui(ns("title"), "Load Data"),
    shinyjs::hidden(checkboxInput(ns("standalone"), NULL, standalone)),

    tabsetPanel(
      id = ns("import_modules"),
      header = tagList(br(), textInput(ns("varname"), "Variable Name", "df", placeholder = "Variable name")),

      tabPanel(
        "Upload",
        value = "upload",
        icon = icon("upload"),
        data_upload_ui(ns("upload"))
      ),
      tabPanel(
        "URL",
        value = "url",
        icon = icon("link"),
        data_url_ui(ns("url"))
      ),
      tabPanel(
        "Datasets",
        value = "dataset",
        icon = icon("table"),
        data_datasets_ui(ns("datasets"))
      ),
      tabPanel(
        "Project Files",
        value = "file",
        icon = icon("folder-open"),
        data_project_files_ui(ns("project_files"))
      ),
    ),
    tags$script(glue::glue("$('#{ns(\"import_modules\")}').addClass('nav-justified');")),
    # shinyWidgets::prettyCheckbox(
    #   ns("custom_name"),
    #   "Custom variable name",
    #   value = FALSE,
    #   shape = "curve",
    #   status = "info"
    # ),
    #conditionalPanel(
    #  "input.custom_name", ns = ns,
    #),
    div(
      shinyWidgets::prettyCheckbox(
        ns("show_code"), "Show Code", value = TRUE, width = "auto", shape = "curve", status = "info", inline = TRUE
      ),
      shinyWidgets::prettyCheckbox(
        ns("show_preview"), "Show Preview", value = TRUE, width = "auto", shape = "curve", status = "info", inline = TRUE
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
      code_chunk_ui(ns("code")),
      br()
    ),
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
}

page_data_select_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      mp_track(
        MIXPANEL_EVENT_INIT,
        list(
          section = MIXPANEL_SECTION_LOAD
        )
      )

      observeEvent(input$import_modules, ignoreInit = TRUE, {
        mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_LOAD,
            type = "select tab",
            tab = input$import_modules
          )
        )
      })

      result <- reactiveValues(name_in = NULL,
                               code_in = NULL,
                               data = NULL,
                               name_out = NULL)

      observeEvent(input$close, {
        kill_app()
      })

      data_upload <- data_upload_server("upload", upload_dir = get_user_upload_dir())
      data_url <- data_url_server("url")
      data_project_files <- data_project_files_server("project_files")
      data_datasets <- data_datasets_server("datasets")

      observeEvent(data_upload$data(), {
        result$name_in <- data_upload$name()
        result$code_in <- data_upload$code()
        result$data <- data_upload$data()
      })

      observeEvent(data_url$data(), {
        result$name_in <- data_url$name()
        result$code_in <- data_url$code()
        result$data <- data_url$data()
      })

      observeEvent(data_project_files$data(), {
        result$name_in <- data_project_files$name()
        result$code_in <- data_project_files$code()
        result$data <- data_project_files$data()
      })

      observeEvent(data_datasets$data(), {
        result$name_in <- data_datasets$name()
        result$code_in <- data_datasets$code()
        result$data <- data_datasets$data()
      })

      name_out <- reactive({
        # req(result$name_in)
        # if (input$custom_name) {
        make.names(input$varname)
        # } else {
        #  result$name_in
        # }
      })

      code_out <- reactive({
        req(result$code_in, name_out())
        paste0(name_out(), " <- ", result$code_in)
      })

      code_chunk_server("code", code_out)

      output$preview_data <- renderTable({
        req(result$data)
        head(result$data, 5)
      }, striped = TRUE, bordered = TRUE, spacing = "xs")

      observe({
        shinyjs::toggleState("continue", condition = (!is.null(result$data) && nrow(result$data) > 0))
        shinyjs::toggleState("insert_code", condition = (!is.null(result$code_in) && !is.null(name_out()) && result$code_in != name_out()))
      })

      observeEvent(input$continue, {
        assign(name_out(), result$data, envir = .GlobalEnv)

        if (input$insert_code) {
          if (result$code_in != name_out()) {
            insert_text(code_out())
          }

          mp_track(
            MIXPANEL_EVENT_CODE,
            list(
              section = MIXPANEL_SECTION_LOAD,
              data_type = input$import_modules
            )
          )
        }

        result$name_out <- name_out()

        if (input$standalone) {
          kill_app()
        }
      })

      return(list(
        done = reactive(input$continue),
        name = reactive(result$name_out)
      ))
    }
  )
}

