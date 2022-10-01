data_url_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert("Enter a URL of a file", status = "info"),
    textInput(ns("url"), NULL, "", placeholder = "https://path/to/data.csv", width = "100%")
  )
}

data_url_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL)

      name <- reactive({
        req(input$url)
        tools::file_path_sans_ext(basename(input$url))
      })

      code <- reactive({
        req(input$url)
        url <- gsub('\\\\', '/', input$url)
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
