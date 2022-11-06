LoadModuleURL <- R6::R6Class(
  "LoadModuleURL",
  inherit = LoadModuleFile
)

LoadModuleURL$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      br(),
      shinyWidgets::alert(
        "Enter a URL of a file with one of the following extensions:", br(),
        paste(LoadModuleFile$FILE_READ_EXTENSIONS, collapse = ", "),
        status = "info", dismissible = TRUE
      ),
      textInput(ns("url"), NULL, "", placeholder = "https://path/to/data.csv", width = "100%")
    )
  },

  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        load <- reactive({
          LoadModuleURL$new(input$url)
        })

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
