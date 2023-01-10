LoadComponentURL <- R6::R6Class(
  "LoadComponentURL",
  inherit = LoadComponentFile
)

LoadComponentURL$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      br(),
      shinyWidgets::alert(
        "Enter the URL of a file with an extension of:",
        paste(LoadComponentFile$FILE_READ_EXTENSIONS, collapse = ", "),
        status = "info", dismissible = TRUE
      ),
      textInput(ns("url"), NULL, "", placeholder = "https://path/to/data.csv", width = "100%"),
      load_file_params_ui(ns("params"))
    )
  },

  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        load <- reactive({
          LoadComponentURL$new(input$url, params = params())
        })

        params <- load_file_params_server("params", file_path = reactive(input$url))

        return(list(
          name = reactive(load()$name),
          data = reactive(load()$data),
          code = reactive(load()$code),
          error = reactive(load()$error)
        ))
      }
    )
  }
)
