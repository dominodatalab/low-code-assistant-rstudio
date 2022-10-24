data_project_files_ui <- function(id) {
  ns <- NS(id)

  tagList(
    file_browser_ui(ns("filebrowser")), br()
  )
}

data_project_files_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL, error = NULL)

      browser <- file_browser_server(
        "filebrowser",
        path = get_user_project_dir(),
        extensions = FILE_READ_EXTENSIONS,
        root = get_user_project_dir(),
        include_empty = FALSE
      )

      name <- reactive({
        req(browser$selected())
        make.names(tools::file_path_sans_ext(basename(browser$selected())))
      })

      code <- reactive({
        req(browser$selected())
        glue::glue("read.csv({shQuote(browser$selected(), type = 'cmd')})")
      })

      observeEvent(code(), {
        tryCatch({
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
