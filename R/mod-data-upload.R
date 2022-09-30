data_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert("Choose a file", status = "info"),
    fileInput(ns("file"), NULL, multiple = FALSE, accept = FILE_READ_EXTENSIONS, width = 500)
  )
}

data_upload_server <- function(id, upload_dir = NULL) {

  if (!is.null(upload_dir) && !dir.exists(upload_dir)) {
    dir.create(upload_dir, recursive = TRUE)
  }

  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL)

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
          file.rename(input$file$datapath, new_path())
        }
      })

      code <- reactive({
        req(input$file)
        url <- gsub('\\\\', '/', new_path())
        glue::glue("read.csv({shQuote(url, type = 'cmd')})")
      })

      observeEvent(code(), {
        data <- eval(parse(text = code()))
        result$name <- name()
        result$code <- code()
        result$data <- data
      })

      return(list(
        name = reactive(result$name),
        data = reactive(result$data),
        code = reactive(result$code)
      ))
    }
  )
}
