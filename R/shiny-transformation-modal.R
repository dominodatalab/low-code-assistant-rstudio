transformation_modal <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      modal_trigger <- reactive_trigger()
      data <- reactiveVal(NULL)
      action <- reactiveVal(NULL)
      xform <- reactiveVal(NULL)
      meta <- reactiveVal(NULL)

      result_xform <- reactiveVal(NULL)

      dialog <-
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
                ns("xform_type"), label = NULL,
                choices = c("Select columns" = "select",
                            "Remove columns" = "drop",
                            "Filter rows" = "filter")
              )
            )
          ),
          conditionalPanel(
            "input.xform_type == 'drop'", ns = ns,
            fluidRow(
              column(4, selectInput(ns("drop_cols"), "Columns", NULL, multiple = TRUE)),
              column(2, textInput(ns("drop_name"), "New name", "df"))
            )
          ),
          conditionalPanel(
            "input.xform_type == 'select'", ns = ns,
            fluidRow(
              column(4, selectInput(ns("select_cols"), "Columns", NULL, multiple = TRUE)),
              column(2, textInput(ns("select_name"), "New name", "df"))
            )
          ),
          conditionalPanel(
            "input.xform_type == 'filter'", ns = ns,
            fluidRow(
              column(4, selectInput(ns("filter_col"), "Column", NULL)),
              column(2, selectInput(ns("filter_op"), "Operation", unname(FilterTransformation$OPTIONS))),
              column(2, textInput(ns("filter_value"), "Value", "")),
              column(2, textInput(ns("filter_name"), "New name", "df"))
            )
          ),
        )

      show <- function(data, action = c("add", "insert", "edit"), xform = NULL, meta = NULL) {
        action <- match.arg(action)

        if (action == "edit" && is.null(xform)) {
          stop("Must provide a transformation for edit", call. = FALSE)
        }

        data(data)
        action(action)
        xform(xform)
        meta(meta)

        modal_trigger$trigger()
      }

      observe({
        req(modal_trigger$depend() > 0)
        showModal(dialog)

        shinyjs::html("title", paste0(firstup(action()), " transformation"))

        updateSelectInput(session, "drop_cols", choices = names(data()))
        updateSelectInput(session, "select_cols", choices = names(data()))
        updateSelectInput(session, "filter_col", choices = names(data()))

        if (!is.null(xform())) {
          shinyjs::hide("xform_type")

          if (inherits(xform(), SelectTransformation$classname)) {
            updateSelectInput(session, "xform_type", selected = "select")
            updateSelectInput(session, "select_cols", selected = xform()$cols)
            updateTextInput(session, "select_name", value = xform()$name_out)
          } else if (inherits(xform(), DropTransformation$classname)) {
            updateSelectInput(session, "xform_type", selected = "drop")
            updateSelectInput(session, "drop_cols", selected = xform()$cols)
            updateTextInput(session, "drop_name", value = xform()$name_out)
          } else if (inherits(xform(), FilterTransformation$classname)) {
            updateSelectInput(session, "xform_type", selected = "filter")
            updateSelectInput(session, "filter_col", selected = xform()$col)
            updateSelectInput(session, "filter_op", selected = xform()$op)
            updateTextInput(session, "filter_value", value = xform()$value)
            updateTextInput(session, "filter_name", value = xform()$name_out)
          } else {
            stop("Unsupported edit type: ", paste(class(xform()), collapse = ", "), call. = FALSE)
          }
        }
      })

      validated <- reactive({
        req(input$xform_type)

        if (input$xform_type == "drop") {
          return(
            length(input$drop_cols) > 0 && isValidName(input$drop_name)
          )
        }
        if (input$xform_type == "select") {
          return(
            length(input$select_cols) > 0 && isValidName(input$select_name)
          )
        }
        if (input$xform_type == "filter") {
          return(
            isValidName(input$filter_name)
          )
        }
        stop("Unsupported type: ", input$xform_type, call. = FALSE)
      })

      observe({
        shinyjs::toggleState("apply", condition = validated())
      })

      observeEvent(input$apply, {
        if (input$xform_type == "drop") {
          action <- DropTransformation$new(cols = input$drop_cols, name_out = input$drop_name)
        } else if (input$xform_type == "select") {
          action <- SelectTransformation$new(cols = input$select_cols, name_out = input$select_name)
        } else if (input$xform_type == "filter") {
          col_type <- class(data()[[input$filter_col]])
          action <- FilterTransformation$new(col = input$filter_col, op = input$filter_op, value = input$filter_value, type = col_type, name_out = input$filter_name)
        } else {
          stop("Unsupported type: ", input$xform_type, call. = FALSE)
        }
        result_xform(action)
        removeModal()
      })

      return(
        list(
          result = result_xform,
          show = show,
          action = action,
          meta = meta
        )
      )
    }
  )
}
