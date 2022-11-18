FilterTransformation <- R6::R6Class(
  "FilterTransformation",
  inherit = Transformation,

  private = list(
    .col = NULL,
    .op = NULL,
    .value = NULL,
    .type = NULL,

    get_dependencies = function() {
      if (private$.tidyverse) "dplyr" else NULL
    },

    get_full_code = function(name_in) {
      value <- self$value
      if (!is.null(private$.type) && private$.type %in% c("character", "factor")) {
        value <- glue::glue('{shQuote(value, type = "cmd")}')
      }

      if (private$.tidyverse) {
        glue::glue(
          '{name_in} %>% ',
          'filter({self$col} {self$op} {value})'
        )
      } else {
        glue::glue(
          '{name_in}',
          '[ {name_in}[["{self$col}"]] {self$op} {value}, , drop = FALSE]'
        )
      }
    }
  ),

  active = list(
    col = function() {
      private$.col
    },
    op = function() {
      private$.op
    },
    value = function() {
      private$.value
    }
  ),

  public = list(

    initialize = function(col, op, value, type = NULL, name_out = NULL, tidyverse = NULL) {
      if (length(col) != 1) {
        stop("You must provide exactly one column", call. = FALSE)
      }
      if (! op %in% FilterTransformation$OPTIONS) {
        stop("The operation must be one of: ", paste(FilterTransformation$OPTIONS, collapse = " "), call. = FALSE)
      }
      super$initialize(name_out, tidyverse)
      private$.col <- col
      private$.op <- op
      private$.value <- value
      private$.type <- type
      invisible(self)
    },

    print = function() {
      super$print()
      cat0(glue::glue("{self$col} {self$op} {self$value}",
                      if (is.null(private$.type)) "" else " ({private$.type})"), "\n")
    }
  )
)

FilterTransformation$OPTIONS <- list(
  LESS_THAN    = "<",
  GREATER_THAN = ">",
  LESS_EQ      = "<=",
  GREATER_EQ   = ">=",
  EQUALS       = "==",
  NOT_EQUALS   = "!="
)

FilterTransformation$name_short <- "filter"
FilterTransformation$name_long <- "Filter rows"

FilterTransformation$shiny <- list(

  ui = function(id) {
    ns <- NS(id)

    fluidRow(
      column(4, shinyWidgets::pickerInput(ns("filter_col"), "Column", NULL)),
      column(2, shinyWidgets::pickerInput(ns("filter_op"), "Operation", unname(FilterTransformation$OPTIONS))),
      column(2, textInput(ns("filter_value"), "Value", "")),
      column(2, textInput(ns("filter_name"), "New name", "df"))
    )
  },

  server = function(id, data, old_xform) {
    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(data(), {
          shinyWidgets::updatePickerInput(session, "filter_col", choices = names(data()))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), FilterTransformation$classname)) {
            shinyWidgets::updatePickerInput(session, "filter_col", selected = old_xform()$col)
            shinyWidgets::updatePickerInput(session, "filter_op", selected = old_xform()$op)
            updateTextInput(session, "filter_value", value = old_xform()$value)
            updateTextInput(session, "filter_name", value = old_xform()$name_out)
          }
        })

        validate <- reactive({
          is_valid_name(input$filter_name)
        })

        xform <- reactive({
          col_type <- class(data()[[input$filter_col]])
          FilterTransformation$new(col = input$filter_col, op = input$filter_op, value = input$filter_value, type = col_type, name_out = input$filter_name)
        })

        return(list(
          validate = validate,
          xform = xform
        ))

      }
    )
  }
)
