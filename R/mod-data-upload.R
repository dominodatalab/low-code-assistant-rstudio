data_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert(
      "Choose a file one of the following extensions: ",
      paste(FILE_READ_EXTENSIONS, collapse = ", "),
      status = "info"
    ),
    fileInput(ns("file"), NULL, multiple = FALSE, accept = FILE_READ_EXTENSIONS, width = "100%")
  )
}

data_upload_server <- function(id, upload_dir = NULL) {

  if (!is.null(upload_dir) && !dir.exists(upload_dir)) {
    dir.create(upload_dir, recursive = TRUE)
  }

  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL, error = NULL)

      name <- reactive({
        req(input$file)
        make.names(tools::file_path_sans_ext(input$file$name))
      })

      new_path <- reactive({
        req(input$file)
        file.path(upload_dir, input$file$name)
      })

      observeEvent(input$file, {
        if (!is.null(upload_dir)) {
          file.copy(input$file$datapath, new_path(), overwrite = TRUE)
        }
      })

      code <- reactive({
        req(input$file)
        url <- gsub('\\\\', '/', new_path())
        glue::glue("read.csv({shQuote(url, type = 'cmd')})")
      })

      observeEvent(code(), {
        tryCatch({
          ext <- paste0(".", tools::file_ext(new_path()))
          if (!ext %in% FILE_READ_EXTENSIONS) {
            stop("file type '", ext, "' not supported")
          }
          data <- suppressWarnings(eval(parse(text = code())))
          result$data <- data
          result$name <- name()
          result$code <- code()
          result$error <- NULL
        }, error = function(err) {
          result$data <- NULL
          result$name <- NULL
          result$code <- NULL
          result$error <- glue::glue("Could not read file ({ err$message })")
        })
      })

      return(list(
        name = reactive(result$name),
        data = reactive(result$data),
        code = reactive(result$code),
        error = reactive(result$error)
      ))
    }
  )
}
