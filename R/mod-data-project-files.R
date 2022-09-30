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

      result <- reactiveValues(name = NULL, code = NULL, data = NULL)

      browser <- file_browser_server(
        "filebrowser",
        path = get_user_project_dir(),
        extensions = FILE_READ_EXTENSIONS,
        allow_back = FALSE,
        show_empty = FALSE
      )

      name <- reactive({
        req(browser$selected())
        make.names(tools::file_path_sans_ext(basename(browser$selected())))
      })

      code <- reactive({
        req(browser$selected())
        url <- gsub('\\\\', '/', browser$selected())
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
