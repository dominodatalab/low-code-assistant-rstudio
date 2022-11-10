LoadModuleUpload <- R6::R6Class(
  "LoadModuleUpload",
  inherit = LoadModuleFile
)

LoadModuleUpload$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      br(),
      shinyWidgets::alert(
        "Choose a file with one of the following extensions:", br(),
        paste(LoadModuleFile$FILE_READ_EXTENSIONS, collapse = ", "),
        status = "info", dismissible = TRUE
      ),
      fileInput(ns("file"), NULL, multiple = FALSE, accept = LoadModuleFile$FILE_READ_EXTENSIONS, width = "100%")
    )
  },

  server = function(id, upload_dir = getwd()) {

    if (!dir.exists(upload_dir)) {
      dir.create(upload_dir, recursive = TRUE)
    }

    moduleServer(
      id,
      function(input, output, session) {
        new_path <- reactive({
          req(input$file)
          file.path(upload_dir, input$file$name)
        })

        observeEvent(input$file, {
          file.copy(input$file$datapath, new_path(), overwrite = TRUE)
        })

        load <- reactive({
          LoadModuleUpload$new(new_path())
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
