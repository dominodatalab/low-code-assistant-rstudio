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
      test_domains = MIXPANEL_TEST_DOMAINS
    ),
    shinyjs::useShinyjs(),
    html_dependency_lca(),
    title_bar_ui(ns("title"), "Load Data"),

    tabsetPanel(
      id = ns("import_modules"),
      header = tagList(br(), textInput(ns("varname"), "Variable Name", "df", placeholder = "Variable name")),

      tabPanel(
        "Upload",
        value = "upload",
        icon = icon("upload"),
        LoadModuleUpload$shiny$ui(ns("upload"))
      ),
      tabPanel(
        "URL",
        value = "url",
        icon = icon("link"),
        LoadModuleURL$shiny$ui(ns("url"))
      ),
      tabPanel(
        "Datasets",
        value = "dataset",
        icon = icon("table"),
        LoadModuleDatasets$shiny$ui(ns("datasets"))
      ),
      tabPanel(
        "Project Files",
        value = "file",
        icon = icon("folder-open"),
        LoadModuleProjectFile$shiny$ui(ns("project_files"))
      ),
      tabPanel(
        "Demo Data",
        value = "demo",
        icon = icon("file"),
        LoadModuleDemo$shiny$ui(ns("demo"))
      )
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
    uiOutput(ns("error")),
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
      actionButton(
        ns("continue"),
        "Apply",
        icon = icon("check"),
        class = "btn-primary btn-lg"
      )
    ),
    br()
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
        upload = LoadModuleUpload$shiny$server("upload", upload_dir = get_user_upload_dir()),
        url = LoadModuleURL$shiny$server("url"),
        dataset = LoadModuleDatasets$shiny$server("datasets"),
        file = LoadModuleProjectFile$shiny$server("project_files"),
        demo = LoadModuleDemo$shiny$server("demo")
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

      name_out <- reactive({
        # req(result$name_in)
        # if (input$custom_name) {
        make.names(input$varname)
        # } else {
        #  result$name_in
        # }
      })

      output$error <- renderUI({
        req(error())
        div(class = "alert alert-danger", icon("exclamation-sign", lib = "glyphicon"), error())
      })

      code_out <- reactive({
        req(code_in(), name_out())
        paste0(name_out(), " <- ", code_in())
      })

      code_chunk_server("code", code_out)

      output$preview_data <- renderTable({
        req(data())
        utils::head(data(), 5)
      }, striped = TRUE, bordered = TRUE, spacing = "xs")

      observe({
        shinyjs::toggleState("continue", condition = (!is.null(data()) && nrow(data()) > 0))
      })

      observeEvent(input$continue, {
        assign(name_out(), data(), envir = .GlobalEnv)

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
