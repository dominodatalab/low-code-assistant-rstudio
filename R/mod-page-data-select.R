page_data_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("data_selector_page"),
      br(),
      radioButtons(ns("datatype"), NULL, inline = FALSE,
                   list(
                     "Upload a dataset" = "upload",
                     "Data from your environment" = "existing"
                   )
      ),
      conditionalPanel(
        ns = ns,
        "input.datatype == 'upload'",
        fileInput(ns("file"), NULL, accept = ".csv")
      ),
      conditionalPanel(
        ns = ns,
        "input.datatype == 'existing'",
        selectInput(ns("data"), NULL, NULL)
      )
    )
  )
}

page_data_select_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      dataname <- reactiveVal(NULL)

      updateSelectInput(session, "data", choices = c("", names(Filter(function(x) is(x, "data.frame"), mget(envir = .GlobalEnv, ls(envir = .GlobalEnv))))))

      observeEvent(input$file, {
        data <- read.csv(input$file$datapath)
        name <- tools::file_path_sans_ext(input$file$name)
        assign(name, data, envir = .GlobalEnv)
        dataname(name)
      })

      observeEvent(input$data, {
        req(nzchar(input$data))
        dataname(input$data)
      })

      return(dataname)

    }
  )
}
