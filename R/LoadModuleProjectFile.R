LoadModuleProjectFile <- R6::R6Class(
  "LoadModuleProjectFile",
  inherit = LoadModuleFile
)

LoadModuleProjectFile$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      shinyfilebrowser::file_browser_ui(ns("filebrowser")), br()
    )
  },

  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        browser <- shinyfilebrowser::file_browser_server(
          "filebrowser",
          path = get_user_project_dir(),
          extensions = LoadModuleFile$FILE_READ_EXTENSIONS,
          root = get_user_project_dir(),
          include_empty = FALSE
        )

        load <- reactive({
          LoadModuleProjectFile$new(browser$selected())
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
