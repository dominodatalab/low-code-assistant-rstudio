LoadModuleDatasets <- R6::R6Class(
  "LoadModuleDatasets",
  inherit = LoadModuleFile
)

LoadModuleDatasets$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      file_browser_ui(ns("filebrowser")), br()
    )
  },

  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        browser <- file_browser_server(
          "filebrowser",
          path = get_user_datasets_dir(),
          extensions = LoadModuleFile$FILE_READ_EXTENSIONS,
          root = get_user_datasets_dir(),
          include_empty = FALSE
        )

        load <- reactive({
          LoadModuleDatasets$new(browser$selected())
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
