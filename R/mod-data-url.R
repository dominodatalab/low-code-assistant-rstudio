data_url_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert("Enter a URL of a CSV file", status = "info"),
    textInput(ns("url"), NULL, "", placeholder = "https://path/to/data.csv", width = "500"),
    shinyWidgets::prettyCheckbox(ns("custom_name"), "Custom variable name", FALSE, shape = "curve", status = "info"),
    conditionalPanel(
      "input.custom_name", ns = ns,
      textInput(ns("varname"), NULL, "", placeholder = "Variable name")
    )
  )
}

data_url_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL)

      name <- reactive({
        req(input$url)

        if (input$custom_name) {
          make.names(input$varname)
        } else {
          tools::file_path_sans_ext(basename(input$url))
        }
      })

      code <- reactive({
        req(input$url)
        url <- gsub('\\\\', '/', input$url)
        glue::glue("{name()} <- read.csv({shQuote(url, type = 'cmd')})")
      })

      observeEvent(code(), {
        data <- eval(parse(text = code()))
        result$name <- name()
        result$code <- code()
        result$data <- data
      })

      return(reactive(list(
        name = result$name,
        data = result$data,
        code = result$code
      )))
    }
  )
}
