page_data_select_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    html_dependency_lca(),
    tabsetPanel(
      id = ns("import_modules"),
      tabPanel(
        "Upload",
        value = "upload",
        icon = icon("upload"),
        br(),
        data_upload_ui(ns("upload"))
      ),
      tabPanel(
        "Environment",
        value = "environment",
        icon = icon("list"),
        br(),
        data_environment_ui(ns("environment"))
      ),
      tabPanel(
        "URL",
        value = "url",
        icon = icon("link"),
        br(),
        data_url_ui(ns("url"))
      ),
      tabPanel(
        "Data Sources",
        value = "database",
        icon = icon("database"),
        br(),
        h1("Not implemented")
      ),
      tabPanel(
        "Datasets",
        value = "datasets",
        icon = icon("table"),
        br(),
        h1("Not implemented")
      ),
      tabPanel(
        "Project Files",
        value = "project",
        icon = icon("folder-tree"),
        br(),
        h1("Not implemented")
      ),
    ),
    tags$script(glue::glue("$('#{ns(\"import_modules\")}').addClass('nav-justified');")),
    shinyWidgets::prettyCheckbox(
      ns("custom_name"),
      "Custom variable name",
      value = FALSE,
      shape = "curve",
      status = "info"
    ),
    conditionalPanel(
      "input.custom_name", ns = ns,
      textInput(ns("varname"), NULL, "df", placeholder = "Variable name")
    ),
    shinyWidgets::prettyCheckbox(
      ns("show_preview"),
      "Show Preview",
      value = TRUE,
      width = "auto",
      shape = "curve",
      status = "info"
    ),
    conditionalPanel(
      "input.show_preview", ns = ns,
      htmltools::tagAppendAttributes(
        tableOutput(ns("preview_data")),
        class = "no-margin small-table"
      ),
      br()
    ),
    div(
      class = "no-margin flex flex-gap2",
      actionButton(ns("close"), "Close"),
      htmltools::tagAppendAttributes(
        shinyWidgets::prettyCheckbox(
          ns("insert_code"),
          "Insert Code",
          value = TRUE,
          width = "auto",
          shape = "curve",
          status = "primary"
        ),
        class = "flex-push"
      ),
      actionButton(
        ns("continue"),
        "Continue",
        icon = icon("angle-double-right"),
        class = "btn-primary btn-lg"
      )
    )
  )
}

page_data_select_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      result <- reactiveValues(name_in = NULL,
                               code_in = NULL,
                               data = NULL,
                               name_out = NULL)

      observeEvent(input$close, {
        kill_app()
      })

      data_upload <- data_upload_server("upload")
      data_env <- data_environment_server("environment")
      data_url <- data_url_server("url")

      observeEvent(data_upload$data(), {
        result$name_in <- data_upload$name()
        result$code_in <- data_upload$code()
        result$data <- data_upload$data()
      })

      observeEvent(data_env$data(), {
        result$name_in <- data_env$name()
        result$code_in <- data_env$code()
        result$data <- data_env$data()
      })

      observeEvent(data_url$data(), {
        result$name_in <- data_url$name()
        result$code_in <- data_url$code()
        result$data <- data_url$data()
      })

      name_out <- reactive({
        req(result$name_in)
        if (input$custom_name) {
          make.names(input$varname)
        } else {
          result$name_in
        }
      })

      code_out <- reactive({
        req(result$code_in, name_out())
        paste0(name_out(), " <- ", result$code_in, "\n")
      })

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
            id <- rstudioapi::getSourceEditorContext()$id
            if (is.null(id)) {
              id <- rstudioapi::documentNew("")
            }
            rstudioapi::insertText(id = id, text = code_out())
          }
        }

        result$name_out <- name_out()
      })

      return(list(
        done = reactive(input$continue),
        name = reactive(result$name_out)
      ))
    }
  )
}
