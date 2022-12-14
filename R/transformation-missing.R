MissingValuesTransformation <- R6::R6Class(
  "MissingValuesTransformation",
  inherit = Transformation,

  private = list(
    .col = NULL,

    get_dependencies = function() {
      if (private$.tidyverse) "tidyr" else NULL
    },

    get_full_code = function(name_in) {
      if (private$.tidyverse) {
        if (is.null(self$col)) {
          filter_code <- 'drop_na()'
        } else {
          filter_code <- 'drop_na("{self$col}")'
        }
        glue::glue(
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
          '{name_in}',
          filter_code
        )
      }
    }
  ),

  active = list(
    col = function() {
      private$.col
    }
  ),

  public = list(

    initialize = function(col = NULL, name_out = NULL, tidyverse = NULL) {
      if (length(col) == 0) {
        col <- NULL
      } else if (length(col) > 1) {
        stop("You must provide exactly one column", call. = FALSE)
      }
      super$initialize(name_out, tidyverse)
      private$.col <- col
      invisible(self)
    },

    print = function() {
      super$print()
      if (is.null(self$col)) {
        cat0("Any rows with missing values", "\n")
      } else {
        cat0("Rows where column `", self$col, "` is missing\n")
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
        shinyWidgets::pickerInput(ns("missing_type"), "Missing values in", choices = c(
          "A specific column" = "column",
          "Any column" = "all"
        ))
      ),
      conditionalPanel(
        "input.missing_type == 'column'", ns = ns,
        column(4, shinyWidgets::pickerInput(ns("missing_col"), "Column", NULL))
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
          shinyWidgets::updatePickerInput(session, "missing_col", choices = names(data()))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), MissingValuesTransformation$classname)) {
            if (is.null(old_xform()$col)) {
              shinyWidgets::updatePickerInput(session, "missing_type", selected = "all")
            } else {
              shinyWidgets::updatePickerInput(session, "missing_type", selected = "column")
              shinyWidgets::updatePickerInput(session, "missing_col", selected = old_xform()$col)
            }
            updateTextInput(session, "missing_name", value = old_xform()$name_out)
          }
        })

        observeEvent(input$missing_name, {
          if (!is_valid_name(input$missing_name)) {
            updateTextInput(session, "missing_name", value = make.names(input$missing_name))
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
