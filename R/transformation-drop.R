DropTransformation <- R6::R6Class(
  "DropTransformation",
  inherit = Transformation,

  private = list(
    .cols = NULL
  ),

  active = list(
    cols = function() {
      private$.cols
    }
  ),

  public = list(

    initialize = function(cols, name_out = NULL) {
      if (length(cols) == 0) {
        stop("You must provide at least one column", call. = FALSE)
      }
      super$initialize(name_out)
      private$.cols <- cols
      invisible(self)
    },

    print = function() {
      cat0("<Transformation> Drop columns: ", paste(self$cols, collapse = ", "), "\n")
    },

    get_code = function(name_in, tidyverse = FALSE) {
      if (tidyverse) {
        glue::glue(
          'library(dplyr)\n',
          '{self$name_out} <- ',
          '{name_in} %>% ',
          'select(-c("{paste(self$cols, collapse = \'", "\')}"))'
        )
      } else {
        glue::glue(
          '{self$name_out} <- ',
          '{name_in}',
          '[, !(names({name_in}) %in% c("{paste(self$cols, collapse = \'", "\')}")), drop = FALSE]'
        )
      }
    }

  )
)

DropTransformation$name_short <- "drop"
DropTransformation$name_long <- "Remove columns"

DropTransformation$shiny <- list(

  ui = function(id) {
    ns <- NS(id)

    fluidRow(
      column(4, selectInput(ns("drop_cols"), "Columns", NULL, multiple = TRUE)),
      column(2, textInput(ns("drop_name"), "New name", "df"))
    )
  },

  server = function(id, data, old_xform) {
    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(data(), {
          updateSelectInput(session, "drop_cols", choices = names(data()))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), DropTransformation$classname)) {
            updateSelectInput(session, "drop_cols", selected = old_xform()$cols)
            updateTextInput(session, "drop_name", value = old_xform()$name_out)
          }
        })

        validate <- reactive({
          length(input$drop_cols) > 0 && is_valid_name(input$drop_name)
        })

        xform <- reactive({
          DropTransformation$new(cols = input$drop_cols, name_out = input$drop_name)
        })

        return(list(
          validate = validate,
          xform = xform
        ))

      }
    )
  }
)
