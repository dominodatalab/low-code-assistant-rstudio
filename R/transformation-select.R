SelectTransformation <- R6::R6Class(
  "SelectTransformation",
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
      cat0("<Transformation> Select columns: ", paste(self$cols, collapse = ", "), "\n")
    },

    get_code = function(name_in) {
      glue::glue(
        '{self$name_out} <- ',
        '{name_in}',
        '[c("{paste(self$cols, collapse = \'", "\')}")]'
      )
    }

  )
)

SelectTransformation$name_short <- "select"
SelectTransformation$name_long <- "Select columns"

SelectTransformation$shiny <- list(

  ui = function(id) {
    ns <- NS(id)

    fluidRow(
      column(4, selectInput(ns("select_cols"), "Columns", NULL, multiple = TRUE)),
      column(2, textInput(ns("select_name"), "New name", "df"))
    )
  },

  server = function(id, data, old_xform) {
    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(data(), {
          updateSelectInput(session, "select_cols", choices = names(data()))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), SelectTransformation$classname)) {
            updateSelectInput(session, "select_cols", selected = old_xform()$cols)
            updateTextInput(session, "select_name", value = old_xform()$name_out)
          }
        })

        validate <- reactive({
          length(input$select_cols) > 0 && isValidName(input$select_name)
        })

        xform <- reactive({
          SelectTransformation$new(cols = input$select_cols, name_out = input$select_name)
        })

        return(list(
          validate = validate,
          xform = xform
        ))

      }
    )
  }
)