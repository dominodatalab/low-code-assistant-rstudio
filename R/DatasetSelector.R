DatasetSelector <- R6Class(
  "DatasetSelector",
  public = list(
    id = NULL,


    initialize = function(id) {
      self$id <- id
    },

    ui = function() {
      ns <- NS(self$id)

      tagList(
        textInput(ns("df_name", "Variable name", "df")),
        tabsetPanel(
          tabPanel(
            "DATA SOURCES",

          ),
          tabPanel(

          )
        ),
        actionButton(ns("tt"), "tt")
      )
    },

    server = function(param) {
      moduleServer(
        self$id,
        function(input, output, session) {
          message(param)
          observeEvent(input$tt, message("YO"))

          return(list(
            "num" = reactive(input$tt)
          ))
        }
      )
    }
  )
)

data_sources_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
     selectInput(ns("source"), "Data Source", NULL),
     actionButton(ns("apply"), "Apply", class = "btn-primary")
  )
}

data_sources_section <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    }
  )
}


library(shiny)

ui <- fluidPage(
  data_sources_section_ui("f")
)

server <- function(input, output, session) {
  data_sources_section("f")
}

shinyApp(ui, server)
