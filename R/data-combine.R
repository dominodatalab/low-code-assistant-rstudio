# # library(shiny)
# #
# # ui <- fluidPage(
# #   esquisse::esquisse_ui("test")
# # )
# #
# # server <- function(input, output, session) {
# #   esquisse::esquisse_server("test")
# # }
# #
# # shinyApp(ui, server)
#
# library(shiny)
#
# ui <- fluidPage(
#   datamods::import_file_ui("a")
# )
#
# server <- function(input, output, session) {
#   datamods::import_file_server("a")
# }
#
#
#
#
#
#
#
#
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   html_dependency_lca(),
#   tabsetPanel(
#     id = ns("import-modules"),
#     tabPanel(
#       "Upload",
#       value = "upload",
#       icon = icon("upload"),
#       br(),
#       data_upload_ui(ns("upload"))
#     ),
#     tabPanel(
#       "Environment",
#       value = "environment",
#       icon = icon("list"),
#       br(),
#       data_environment_ui(ns("environment"))
#     ),
#     tabPanel(
#       "URL",
#       value = "url",
#       icon = icon("link"),
#       br(),
#       data_url_ui(ns("url"))
#     )
#   ),
#   tags$script(glue::glue("$('#{ns(\"import-modules\")}').addClass('nav-justified');")),
#   tags$hr(),
#   shinyWidgets::prettyCheckbox(ns("show_preview"), "Show Preview", TRUE, width = "auto", shape = "curve", status = "info"),
#   conditionalPanel(
#     "input.show_preview", ns = ns,
#     htmltools::tagAppendAttributes(
#       tableOutput(ns("preview_data")),
#       class = "no-margin small-table"
#     ),
#     br()
#   ),
#   div(
#     class = "no-margin flex flex-gap2",
#     actionButton(ns("close"), "Close"),
#     htmltools::tagAppendAttributes(
#       shinyWidgets::prettyCheckbox(ns("insert_code"), "Insert Code", TRUE, width = "auto", shape = "curve", status = "primary"),
#       class = "flex-push"
#     ),
#     actionButton(ns("continue"), "Continue", icon = icon("angle-double-right"), class = "btn-primary btn-lg")
#   )
# )
#
# server <- function(input, output, session) {
#
#   result <- reactiveValues(name = NULL, code = NULL, data = NULL)
#
#   observeEvent(input$close, {
#     kill_app()
#   })
#
#   data_upload <- data_upload_server("upload")
#   data_env <- data_environment_server("environment")
#   data_url <- data_url_server("url")
#
#   observeEvent(data_upload(), {
#     result$name <- data_upload()$name
#     result$code <- data_upload()$code
#     result$data <- data_upload()$data
#   })
#
#   observeEvent(data_env(), {
#     result$name <- data_env()$name
#     result$code <- data_env()$code
#     result$data <- data_env()$data
#   })
#
#   observeEvent(data_url(), {
#     result$name <- data_url()$name
#     result$code <- data_url()$code
#     result$data <- data_url()$data
#   })
#
#   output$preview_data <- renderTable({
#     req(result$data)
#     head(result$data, 5)
#   }, striped = TRUE, bordered = TRUE, spacing = "xs")
#
#   observe({
#     shinyjs::toggleState("continue", condition = (!is.null(result$data) && nrow(result$data) > 0))
#     shinyjs::toggleState("insert_code", condition = (!is.null(result$code) && !is.null(result$name) && result$code != result$name))
#   })
#
#   observeEvent(input$continue, {
#     assign(name, data, envir = .GlobalEnv)
#   })
# }
#
# shinyApp(ui, server)
#
