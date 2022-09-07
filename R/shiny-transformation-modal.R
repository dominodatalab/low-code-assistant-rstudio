transformation_modal <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data <- reactiveValEvent(NULL)
      action <- reactiveValEvent(NULL)
      xform <- reactiveValEvent(NULL)
      meta <- reactiveValEvent(NULL)

      result_xform <- reactiveVal(NULL)

      dialog <-
        modalDialog(
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("apply"), "Apply", class = "btn-primary"),
            modalButton("Cancel")
          ),
          shinyjs::useShinyjs(),
          shiny::singleton(tags$head(tags$style(
            ".aggregate_existing_ui .row {position: relative; margin-bottom: 3px;}
             .aggregate_existing_ui .row .remove-btn { cursor: pointer; position: absolute; left: 15px; right: 15px; top: 0; bottom: 0; align-items: center; justify-content: center; z-index: 1; background: #fbe9e9; font-weight: bold; display: none; }
             .aggregate_existing_ui .row:hover .remove-btn { display: flex !important; }"
          ))),
          h2(div(id = ns("title"))),
          fluidRow(
            column(
              12,
              selectInput(
                ns("xform_type"), label = NULL, choices = c(
                  "Select columns" = "select",
                  "Remove columns" = "drop",
                  "Filter rows" = "filter",
                  "Remove rows with missing values" = "missing",
                  "Group & aggregate" = "aggregate"
                )
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
          conditionalPanel(
            "input.xform_type == 'missing'", ns = ns,
            fluidRow(
              column(4, selectInput(ns("missing_type"), "Missing values in", choices = c(
                "A specific column" = "column",
                "Any column" = "all"
              ))),
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
          ),
          conditionalPanel(
            "input.xform_type == 'aggregate'", ns = ns,
            fluidRow(
              column(3, selectInput(ns("aggregate_cols"), "Group by", NULL, multiple = TRUE)),
              column(3, selectInput(ns("aggregate_col_agg"), "Aggregate on", "")),
              column(3, selectInput(ns("aggregate_aggregator"), "Aggregator", c("", unname(AggregateTransformation$OPTIONS)))),
              column(3, textInput(ns("aggregate_name"), "New name", "dfg"))
            ),
            fluidRow(
              column(6, offset = 3, uiOutput(ns("aggregate_existing_ui"), class = "aggregate_existing_ui"))
            ),
            shinyjs::hidden(textInput(ns("aggregate_existing"), "", "[]"))
          )
        )

      show <- function(data, action = c("add", "insert", "edit"), xform = NULL, meta = NULL) {
        action <- match.arg(action)

        if (action == "edit" && is.null(xform)) {
          stop("Must provide a transformation for edit", call. = FALSE)
        }

        showModal(dialog)

        data(data)
        action(action)
        xform(xform)
        meta(meta)
      }

      observeEvent(data(), {
        updateSelectInput(session, "drop_cols", choices = names(data()))
        updateSelectInput(session, "select_cols", choices = names(data()))
        updateSelectInput(session, "filter_col", choices = names(data()))
        updateSelectInput(session, "missing_col", choices = names(data()))
        updateSelectInput(session, "aggregate_cols", choices = names(data()))
        updateSelectInput(session, "aggregate_col_agg", choices = c("", names(data())))
      })

      observeEvent(action(), {
        shinyjs::html("title", paste0(firstup(action()), " transformation"))
      })

      observeEvent(xform(), {
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
          } else if (inherits(xform(), MissingValuesTransformation$classname)) {
            updateSelectInput(session, "xform_type", selected = "missing")
            if (is.null(xform()$col)) {
              updateSelectInput(session, "missing_type", selected = "all")
            } else {
              updateSelectInput(session, "missing_type", selected = "column")
              updateSelectInput(session, "missing_col", selected = xform()$col)
            }
            updateTextInput(session, "missing_name", value = xform()$name_out)
          } else if (inherits(xform(), AggregateTransformation$classname)) {
            aggs <- lapply(seq_along(xform()$aggregations), function(idx) {
              as.list(xform()$aggregations[idx])
            })
            updateSelectInput(session, "xform_type", selected = "aggregate")
            updateSelectInput(session, "aggregate_cols", selected = xform()$cols)
            updateTextInput(session, "aggregate_existing", value = as.character(jsonlite::toJSON(aggs)))
            updateTextInput(session, "aggregate_name", value = xform()$name_out)
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
        if (input$xform_type == "missing") {
          return(
            isValidName(input$missing_name)
          )
        }
        if (input$xform_type == "aggregate") {
          return(
            length(input$aggregate_cols) > 0 && isValidName(input$aggregate_name) &&
              length(existing_aggregations() > 0)
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
        } else if (input$xform_type == "missing") {
          col <- if (input$missing_type == "column") input$missing_col else NULL
          action <- MissingValuesTransformation$new(col = col, name_out = input$missing_name)
        } else if (input$xform_type == "aggregate") {
          action <- AggregateTransformation$new(cols = input$aggregate_cols, aggregations = unlist(existing_aggregations()), name_out = input$aggregate_name)
        } else {
          stop("Unsupported type: ", input$xform_type, call. = FALSE)
        }
        result_xform(action)
        removeModal()
      })

      #--- Logic for groupby/aggregate having multiple groupings
      # A hidden text field saves the existing aggregations as a JSON list-of-lists
      # Note that in JavaScript a dictionary cannot have duplicate keys, which is why
      # we have to use a list-of-lists instead of a single list
      observeEvent(c(input$aggregate_col_agg, input$aggregate_aggregator), {
        req(input$aggregate_col_agg, input$aggregate_aggregator)
        updateSelectInput(session, "aggregate_col_agg", selected = "")
        updateSelectInput(session, "aggregate_aggregator", selected = "")

        new_aggregation <- list(as.list(setNames(input$aggregate_aggregator, input$aggregate_col_agg)))
        if (new_aggregation %in% existing_aggregations()) {
          return()
        }
        new_aggregations <- append(existing_aggregations(), new_aggregation)
        updateTextInput(session, "aggregate_existing", value = as.character(jsonlite::toJSON(new_aggregations)))
      })

      existing_aggregations <- reactive({
        jsonlite::fromJSON(input$aggregate_existing, simplifyDataFrame = FALSE)
      })
      output$aggregate_existing_ui <- renderUI({
        lapply(seq_along(existing_aggregations()), function(idx) {
          fluidRow(
            div(
              "Remove", class="remove-btn",
              onclick = glue::glue("Shiny.setInputValue('{{ns('aggregate_remove')}}', {{idx}}, {priority: 'event'})", .open = "{{", .close = "}}")
            ),
            column(
              6,
              tags$input(
                value = names(existing_aggregations()[[idx]][1]),
                type="text", disabled=NA, class="form-control"
              )
            ),
            column(
              6,
              tags$input(
                value = existing_aggregations()[[idx]][[1]],
                type="text", disabled=NA, class="form-control"
              )
            )
          )
        })
      })
      observeEvent(input$aggregate_remove, {
        aggs <- existing_aggregations()
        aggs[input$aggregate_remove] <- NULL
        updateTextInput(session, "aggregate_existing", value = as.character(jsonlite::toJSON(aggs)))
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
