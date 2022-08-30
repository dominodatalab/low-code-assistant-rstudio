page_data_select_ui <- function(id) {
  ns <- NS(id)

  shinyjs::hidden(
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
        selectInput(ns("data"), NULL, "")
      )
    )
  )
}

page_data_select_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      # The show/hide code can't be placed inside a function and is instead
      # in an observer because of https://github.com/rstudio/shiny/issues/2706
      show_trigger <- reactive_trigger()
      hide_trigger <- reactive_trigger()

      data_name <- reactiveVal(NULL)

      observe({
        req(show_trigger$depend() > 0)
        isolate({
          shinyjs::show("data_selector_page")
          updateSelectInput(session, "data", choices = c("", names(Filter(function(x) is(x, "data.frame"), mget(envir = .GlobalEnv, ls(envir = .GlobalEnv))))))
          data_name(NULL)
        })
      })


      observe({
        req(hide_trigger$depend() > 0)
        isolate({
          shinyjs::hide("data_selector_page")
          shinyjs::reset("data_selector_page")
        })
      })

      observeEvent(input$file, {
        data <- read.csv(input$file$datapath)
        name <- tools::file_path_sans_ext(input$file$name)
        assign(name, data, envir = .GlobalEnv)
        data_name(name)
      })

      observeEvent(input$data, {
        req(nzchar(input$data))
        data_name(input$data)
      })

      return(list(
        show = function() show_trigger$trigger(),
        hide = function() hide_trigger$trigger(),
        data_name = data_name
      ))

    }
  )
}
