data_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert("Choose a CSV file", status = "info"),
    fileInput(ns("file"), NULL, multiple = FALSE, accept = c(".csv"), width = 500),
    shinyWidgets::prettyCheckbox(ns("custom_name"), "Custom variable name", FALSE, shape = "curve", status = "info"),
    conditionalPanel(
      "input.custom_name", ns = ns,
      textInput(ns("varname"), NULL, "", placeholder = "Variable name")
    )
  )
}

data_upload_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL)

      name <- reactive({
        req(input$file)

        if (input$custom_name) {
          make.names(input$varname)
        } else {
          make.names(tools::file_path_sans_ext(input$file$name))
        }
      })

      code <- reactive({
        req(input$file)
        url <- gsub('\\\\', '/', input$file$datapath)
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
