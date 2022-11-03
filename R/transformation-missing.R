MissingValuesTransformation <- R6::R6Class(
  "MissingValuesTransformation",
  inherit = Transformation,

  private = list(
    .col = NULL
  ),

  active = list(
    col = function() {
      private$.col
    }
  ),

  public = list(

    initialize = function(col = NULL, name_out = NULL) {
      if (length(col) == 0) {
        col <- NULL
      } else if (length(col) > 1) {
        stop("You must provide exactly one column", call. = FALSE)
      }
      super$initialize(name_out)
      private$.col <- col
      invisible(self)
    },

    print = function() {
      if (is.null(self$col)) {
        cat0("<Transformation> Missing values: Any rows with missing values", "\n")
      } else {
        cat0("<Transformation> Missing values: Rows where column is missing: ", self$col, "\n")
      }
    },

    get_code = function(name_in, tidyverse = FALSE) {
      if (tidyverse) {
        if (is.null(self$col)) {
          filter_code <- 'drop_na()'
        } else {
          filter_code <- 'drop_na("{self$col}")'
        }
        glue::glue(
          'library(tidyr)\n',
          '{self$name_out} <- ',
          '{name_in} %>% ',
          filter_code
        )
      } else {
        if (is.null(self$col)) {
          filter_code <- '[complete.cases({name_in}), , drop = FALSE]'
        } else {
          filter_code <- '[!is.na({name_in}[["{self$col}"]]), , drop = FALSE]'
        }
        glue::glue(
          '{self$name_out} <- ',
          '{name_in}',
          filter_code
        )
      }
    }

  )
)

MissingValuesTransformation$name_short <- "missing"
MissingValuesTransformation$name_long <- "Remove rows with missing values"

MissingValuesTransformation$shiny <- list(

  ui = function(id) {
    ns <- NS(id)

    fluidRow(
      column(
        4,
        selectInput(ns("missing_type"), "Missing values in", choices = c(
          "A specific column" = "column",
          "Any column" = "all"
        ))
      ),
      conditionalPanel(
        "input.missing_type == 'column'", ns = ns,
        column(4, selectInput(ns("missing_col"), "Column", NULL))
      ),
      conditionalPanel(
        "input.missing_type != 'column'", ns = ns,
        column(4, ""),
      ),
      column(2, textInput(ns("missing_name"), "New name", "df"))
    )
  },

  server = function(id, data, old_xform) {
    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(data(), {
          updateSelectInput(session, "missing_col", choices = names(data()))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), MissingValuesTransformation$classname)) {
            if (is.null(old_xform()$col)) {
              updateSelectInput(session, "missing_type", selected = "all")
            } else {
              updateSelectInput(session, "missing_type", selected = "column")
              updateSelectInput(session, "missing_col", selected = old_xform()$col)
            }
            updateTextInput(session, "missing_name", value = old_xform()$name_out)
          }
        })

        validate <- reactive({
          is_valid_name(input$missing_name)
        })

        xform <- reactive({
          col <- if (input$missing_type == "column") input$missing_col else NULL
          MissingValuesTransformation$new(col = col, name_out = input$missing_name)
        })

        return(list(
          validate = validate,
          xform = xform
        ))

      }
    )
  }
)
