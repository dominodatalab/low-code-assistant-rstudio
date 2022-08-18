TransformationModal <- R6::R6Class(
  "TransformationModal",
  cloneable = FALSE,

  private = list(

    .id = NULL,
    .trigger = NULL,
    .data = NULL,
    .action = NULL,
    .xform = NULL,

    dialog = function() {
      ns <- NS(private$.id)

      modalDialog(
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("apply"), "Apply", class = "btn-primary"),
          modalButton("Cancel")
        ),
        shinyjs::useShinyjs(),
        h2(div(id = ns("title"))),
        fluidRow(
          column(
            12,
            selectInput(
              ns("action_type"), label = NULL,
              choices = c("Select columns" = "select",
                          "Remove columns" = "drop",
                          "Filter rows" = "filter")
            )
          )
        ),
        conditionalPanel(
          "input.action_type == 'drop'", ns = ns,
          fluidRow(
            column(4, selectInput(ns("drop_cols"), "Columns", NULL, multiple = TRUE)),
            column(2, textInput(ns("drop_name"), "New name", "df"))
          )
        ),
        conditionalPanel(
          "input.action_type == 'select'", ns = ns,
          fluidRow(
            column(4, selectInput(ns("select_cols"), "Columns", NULL, multiple = TRUE)),
            column(2, textInput(ns("select_name"), "New name", "df"))
          )
        ),
        conditionalPanel(
          "input.action_type == 'filter'", ns = ns,
          fluidRow(
            column(4, selectInput(ns("filter_col"), "Column", NULL)),
            column(2, selectInput(ns("filter_op"), "Operation", unname(FilterTransformation$OPTIONS))),
            column(2, textInput(ns("filter_value"), "Value", "")),
            column(2, textInput(ns("filter_name"), "New name", "df"))
          )
        ),
      )
    }

  ),

  public = list(

    initialize = function(id) {
      private$.id <- id
      private$.trigger <- reactive_trigger()
      invisible(self)
    },

    show = function(data, action = c("add", "insert", "edit"), xform = NULL) {
      action <- match.arg(action)
      private$.data <- data
      private$.action <- action
      private$.xform <- xform

      if (action == "edit" && is.null(xform)) {
        stop("Must provide a transformation for edit", call. = FALSE)
      }

      private$.trigger$trigger()
    },

    run = function() {
      moduleServer(
        private$.id,
        function(input, output, session) {

          ns <- session$ns(private$.id)

          new_xform <- reactiveVal(NULL)

          observe({
            req(private$.trigger$depend() > 0)
            showModal(private$dialog())

            shinyjs::html("title", paste0(firstup(private$.action), " transformation"))

            updateSelectInput(session, "drop_cols", choices = names(private$.data))
            updateSelectInput(session, "select_cols", choices = names(private$.data))
            updateSelectInput(session, "filter_col", choices = names(private$.data))

            if (!is.null(private$.xform)) {
              xform <- private$.xform
              shinyjs::hide("action_type")

              if (inherits(xform, SelectTransformation$classname)) {
                updateSelectInput(session, "action_type", selected = "select")
                updateSelectInput(session, "select_cols", selected = xform$cols)
                updateTextInput(session, "select_name", value = xform$name_out)
              } else if (inherits(xform, DropTransformation$classname)) {
                updateSelectInput(session, "action_type", selected = "drop")
                updateSelectInput(session, "drop_cols", selected = xform$cols)
                updateTextInput(session, "drop_name", value = xform$name_out)
              } else if (inherits(xform, FilterTransformation$classname)) {
                updateSelectInput(session, "action_type", selected = "filter")
                updateSelectInput(session, "filter_col", selected = xform$col)
                updateSelectInput(session, "filter_op", selected = xform$op)
                updateTextInput(session, "filter_value", value = xform$value)
                updateTextInput(session, "filter_name", value = xform$name_out)
              } else {
                stop("Unsupported edit type: ", paste(class(xform), collapse = ", "), call. = FALSE)
              }
            }
          })

          validated <- reactive({
            req(input$action_type)

            if (input$action_type == "drop") {
              return(
                length(input$drop_cols) > 0 && isValidName(input$drop_name)
              )
            }
            if (input$action_type == "select") {
              return(
                length(input$select_cols) > 0 && isValidName(input$select_name)
              )
            }
            if (input$action_type == "filter") {
              return(
                isValidName(input$filter_name)
              )
            }
            stop("Unsupported type: ", input$action_type, call. = FALSE)
          })

          observe({
            shinyjs::toggleState("apply", condition = validated())
          })

          observeEvent(input$apply, {
            if (input$action_type == "drop") {
              action <- DropTransformation$new(cols = input$drop_cols, name_out = input$drop_name)
            } else if (input$action_type == "select") {
              action <- SelectTransformation$new(cols = input$select_cols, name_out = input$select_name)
            } else if (input$action_type == "filter") {
              col_type <- class(private$.data[[input$filter_col]])
              action <- FilterTransformation$new(col = input$filter_col, op = input$filter_op, value = input$filter_value, type = col_type, name_out = input$filter_name)
            } else {
              stop("Unsupported type: ", input$action_type, call. = FALSE)
            }
            new_xform(action)
            removeModal()
          })

          return(new_xform)
        }
      )
    }

  )
)
