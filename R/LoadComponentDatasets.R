LoadComponentDatasets <- R6::R6Class(
  "LoadComponentDatasets",
  inherit = LoadComponentFile
)

LoadComponentDatasets$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      br(),
      shinyfilebrowser::file_browser_ui(ns("filebrowser")),
      load_file_params_ui(ns("params"))
    )
  },

  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        browser <- shinyfilebrowser::file_browser_server(
          "filebrowser",
          path = get_user_datasets_dir(),
          extensions = LoadComponentFile$FILE_READ_EXTENSIONS,
          root = get_user_datasets_dir(),
          include_empty = FALSE
        )

        load <- reactive({
          LoadComponentDatasets$new(browser$selected(), params = params())
        })

        params <- load_file_params_server("params", file_path = reactive(browser$selected()))

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
