LoadModuleDemo <- R6::R6Class(
  "LoadModuleDemo",
  inherit = LoadModule,

  private = list(
    .dataset = NULL,

    set_dataset = function(x) {
      if (!is.null(x) && x == "") {
        x <- NULL
      }
      private$.dataset <- x

      if (is.null(x)) {
        private$.name <- private$.code <- private$.data <- private$.error <- NULL
      } else {
        private$run()
      }
    },

    get_name = function() {
      private$.dataset
    },

    get_code = function() {
      if (!private$.dataset %in% LoadModuleDemo$DATASETS) {
        stop("Unknown dataset: ", private$.dataset)
      }
      if (private$.dataset %in% c("diamonds", "economics", "midwest")) {
        glue::glue("ggplot2::{ private$.dataset }")
      } else if (private$.dataset %in% c("iris", "mtcars")) {
        glue::glue("datasets::{ private$.dataset }")
      } else if (private$.dataset == "gapminder") {
        "gapminder::gapminder"
      } else if (private$.dataset == "small_molecule_drugbank") {
        glue::glue('read.csv(system.file("data", "{ private$.dataset }.csv", package = "assistDomino"))')
      }
    },

    get_data = function() {
      suppressWarnings(eval(parse(text = private$.code)))
    }
  ),

  public = list(
    initialize = function(dataset = NULL) {
      # If the dataset is the result of a failed shiny req() call, treat it as NULL instead of error
      dataset <- tryCatch(dataset, error = function(err) NULL)

      super$initialize()
      private$set_dataset(dataset)
      invisible(self)
    }
  )
)

LoadModuleDemo$DATASETS <- c("gapminder", "iris", "mtcars", "diamonds", "economics", "midwest", "small_molecule_drugbank")

LoadModuleDemo$shiny <- list(

  ui = function(id) {
    ns <- NS(id)
    tagList(
      selectInput(ns("datasets"), NULL, sort(c("--- Choose a dataset ---" = "", LoadModuleDemo$DATASETS)), ""), br()
    )
  },

  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        load <- reactive({
          LoadModuleDemo$new(input$datasets)
        })

        return(list(
          name = reactive(load()$name),
          data = reactive(load()$data),
          code = reactive(load()$code),
          error = reactive(load()$error)
        ))
      }
    )
  }
)
