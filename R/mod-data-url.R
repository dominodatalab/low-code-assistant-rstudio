data_url_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert(
      "Enter a URL of a file with one of the following extensions:", br(),
      paste(FILE_READ_EXTENSIONS, collapse = ", "),
      status = "info"
    ),
    textInput(ns("url"), NULL, "", placeholder = "https://path/to/data.csv", width = "100%")
  )
}

data_url_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL, error = NULL)

      name <- reactive({
        req(input$url)
        tools::file_path_sans_ext(basename(input$url))
      })

      code <- reactive({
        req(input$url)
        url <- gsub('\\\\', '/', input$url)
        get_load_code(url)
      })

      observeEvent(code(), {
        tryCatch({
          ext <- paste0(".", tools::file_ext(input$url))
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
