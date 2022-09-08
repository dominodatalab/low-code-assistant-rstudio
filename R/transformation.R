Transformation <- R6::R6Class(
  "Transformation",
  cloneable = FALSE,

  private = list(
    .name_out = NULL
  ),

  active = list(
    name_out = function() {
      private$.name_out
    }
  ),

  public = list(
    initialize = function(name_out) {
      private$.name_out <- name_out %||% "df"
      invisible(self)
    }
  )
)

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

    get_code = function(name_in) {
      glue::glue(
        '{self$name_out} <- ',
        '{name_in}',
        '[, !(names({name_in}) %in% c("{paste(self$cols, collapse = \'", "\')}")), drop = FALSE]'
      )
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
          length(input$drop_cols) > 0 && isValidName(input$drop_name)
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

FilterTransformation <- R6::R6Class(
  "FilterTransformation",
  inherit = Transformation,

  private = list(
    .col = NULL,
    .op = NULL,
    .value = NULL,
    .type = NULL
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

    initialize = function(col, op, value, type = NULL, name_out = NULL) {
      if (length(col) != 1) {
        stop("You must provide exactly one column", call. = FALSE)
      }
      if (! op %in% FilterTransformation$OPTIONS) {
        stop("The operation must be one of: ", paste(FilterTransformation$OPTIONS, collapse = " "), call. = FALSE)
      }
      super$initialize(name_out)
      private$.col <- col
      private$.op <- op
      private$.value <- value
      private$.type <- type
      invisible(self)
    },

    print = function() {
      cat0(glue::glue("<Transformation> Filter column: {self$col} {self$op} {self$value}",
                     if (is.null(private$.type)) "" else " ({private$.type})"), "\n")
    },

    get_code = function(name_in) {
      value <- self$value
      if (!is.null(private$.type) && private$.type %in% c("character", "factor")) {
        value <- glue::glue('{shQuote(value, type = "cmd")}')
      }

      glue::glue(
        '{self$name_out} <- ',
        '{name_in}',
        '[ {name_in}[["{self$col}"]] {self$op} {value}, , drop = FALSE]'
      )
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
      column(4, selectInput(ns("filter_col"), "Column", NULL)),
      column(2, selectInput(ns("filter_op"), "Operation", unname(FilterTransformation$OPTIONS))),
      column(2, textInput(ns("filter_value"), "Value", "")),
      column(2, textInput(ns("filter_name"), "New name", "df"))
    )
  },

  server = function(id, data, old_xform) {
    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(data(), {
          updateSelectInput(session, "filter_col", choices = names(data()))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), FilterTransformation$classname)) {
            updateSelectInput(session, "filter_col", selected = old_xform()$col)
            updateSelectInput(session, "filter_op", selected = old_xform()$op)
            updateTextInput(session, "filter_value", value = old_xform()$value)
            updateTextInput(session, "filter_name", value = old_xform()$name_out)
          }
        })

        validate <- reactive({
          isValidName(input$filter_name)
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

    get_code = function(name_in) {
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
          isValidName(input$missing_name)
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


AggregateTransformation <- R6::R6Class(
  "AggregateTransformation",
  inherit = Transformation,

  private = list(

    .cols = NULL,
    .aggregations = NULL,  # named list

    get_code_for_aggregator = function(type, aggr_cols, name_in) {
      if (type == "size") {
        fxn <- "length"
      } else {
        fxn <- type
      }
      glue::glue(
        "aggregate(",
        "cbind(",
        glue::glue_collapse(paste(paste(aggr_cols, type, sep = '_'), aggr_cols, sep = ' = '), sep = ', '),
        ")",
        " ~ ",
        "{glue::glue_collapse(self$cols, sep = ' + ')}, ",
        "{name_in}, ",
        "FUN = {fxn})"
      )
    }

  ),

  active = list(
    cols = function() {
      private$.cols
    },
    aggregations = function() {
      private$.aggregations
    }
  ),

  public = list(

    initialize = function(cols, aggregations, name_out = NULL) {
      if (length(cols) == 0) {
        stop("You must provide at least one column to group by", call. = FALSE)
      }
      if (length(aggregations) == 0) {
        stop("You must provide at least one aggregation", call. = FALSE)
      }
      super$initialize(name_out)
      private$.cols <- cols
      private$.aggregations <- aggregations
      invisible(self)
    },

    print = function() {
      cat0(
        "<Transformation> Group/Aggregate:\n",
        "    Group by: ",
        paste(self$cols, collapse = ", "), "\n",
        "    Aggregate by: ",
        paste(names(self$aggregations), unname(self$aggregations), sep = "=", collapse = ", "), "\n"
      )
    },

    get_code = function(name_in) {
      aggr_types <- unique(self$aggregations)
      aggregations <- lapply(aggr_types, function(type) {
        if (! type %in% AggregateTransformation$OPTIONS) {
          stop("The aggregator must be one of: ", paste(AggregateTransformation$OPTIONS, collapse = " "), " (given: ", type, ")", call. = FALSE)
        }
        aggr_cols <- names(self$aggregations[self$aggregations == type])
        aggr_cols <- unique(aggr_cols)
        private$get_code_for_aggregator(type, aggr_cols, name_in)
      })

      if (length(aggregations) == 1) {
        glue::glue(
          '{self$name_out} <- ',
          '{aggregations[[1]]}'
        )
      } else {
        glue::glue(
          '{self$name_out} <- Reduce(',
          '  merge,',
          '  list(',
          glue::glue_collapse(paste0("    ", aggregations), sep = ",\n"),
          '  )',
          ')',
          .sep = '\n'
        )
      }
    }
  )
)

AggregateTransformation$OPTIONS <- list(
  LENGTH = "size",
  SUM    = "sum",
  MEAN   = "mean",
  MIN    = "min",
  MAX    = "max"
)

AggregateTransformation$name_short <- "aggregate"
AggregateTransformation$name_long <- "Group & aggregate"

AggregateTransformation$shiny <- list(

  ui = function(id) {
    ns <- NS(id)

    tagList(
      shiny::singleton(tags$head(tags$style(
        ".aggregate_existing_ui .row {position: relative; margin-bottom: 3px;}
             .aggregate_existing_ui .row .remove-btn { cursor: pointer; position: absolute; left: 15px; right: 15px; top: 0; bottom: 0; align-items: center; justify-content: center; z-index: 1; background: #fbe9e9; font-weight: bold; display: none; }
             .aggregate_existing_ui .row:hover .remove-btn { display: flex !important; }"
      ))),
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
  },

  server = function(id, data, old_xform) {
    moduleServer(
      id,
      function(input, output, session) {

        ns <- session$ns

        observeEvent(data(), {
          updateSelectInput(session, "aggregate_cols", choices = names(data()))
          updateSelectInput(session, "aggregate_col_agg", choices = c("", names(data())))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), AggregateTransformation$classname)) {
            aggs <- lapply(seq_along(old_xform()$aggregations), function(idx) {
              as.list(old_xform()$aggregations[idx])
            })
            updateSelectInput(session, "xform_type", selected = "aggregate")
            updateSelectInput(session, "aggregate_cols", selected = old_xform()$cols)
            updateTextInput(session, "aggregate_existing", value = as.character(jsonlite::toJSON(aggs)))
            updateTextInput(session, "aggregate_name", value = old_xform()$name_out)
          }
        })

        validate <- reactive({
          length(input$aggregate_cols) > 0 && isValidName(input$aggregate_name) &&
            length(existing_aggregations() > 0)
        })

        xform <- reactive({
          AggregateTransformation$new(cols = input$aggregate_cols, aggregations = unlist(existing_aggregations()), name_out = input$aggregate_name)
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

        return(list(
          validate = validate,
          xform = xform
        ))

      }
    )
  }
)

ALL_XFORMS <- c(
  SelectTransformation,
  DropTransformation,
  FilterTransformation,
  MissingValuesTransformation,
  AggregateTransformation
)
