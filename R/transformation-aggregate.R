AggregateTransformation <- R6::R6Class(
  "AggregateTransformation",
  inherit = Transformation,

  private = list(

    .cols = NULL,
    .aggregations = NULL,  # named list

    get_dependencies = function() {
      if (private$.tidyverse) "dplyr" else NULL
    },

    get_full_code = function(name_in) {
      aggregations <- private$get_code_for_aggregators(name_in)
      if (self$tidyverse) {
        if (length(aggregations) == 1) {
          summaries <- glue::glue("  summarise({aggregations[[1]]})")
        } else {
          summaries <- glue::glue(
            '  summarise(\n',
            '{paste("    ", aggregations, collapse = ",\n")}\n',
            '  )',
            .trim = FALSE
          )
        }
        glue::glue(
          '{name_in} %>%\n  group_by({paste(private$.cols, collapse = ", ")}) %>%\n',
          summaries,
          .trim = FALSE
        )
      } else {
        if (length(aggregations) == 1) {
          glue::glue(
            '{aggregations[[1]]}'
          )
        } else {
          glue::glue(
            'Reduce(',
            '  merge,',
            '  list(',
            glue::glue_collapse(paste0("    ", aggregations), sep = ",\n"),
            '  )',
            ')',
            .sep = '\n'
          )
        }
      }
    },

    get_code_for_aggregators = function(name_in) {
      if (self$tidyverse) {
        aggregations <- lapply(seq_along(self$aggregations), function(idx) {
          type <- self$aggregations[[idx]]
          col <- names(self$aggregations[idx])
          if (! type %in% AggregateTransformation$OPTIONS) {
            stop("The aggregator must be one of: ", paste(AggregateTransformation$OPTIONS, collapse = " "), " (given: ", type, ")", call. = FALSE)
          }
          private$get_code_for_aggregator(type, col, name_in)
        })
      } else {
        aggr_types <- unique(self$aggregations)
        aggregations <- lapply(aggr_types, function(type) {
          if (! type %in% AggregateTransformation$OPTIONS) {
            stop("The aggregator must be one of: ", paste(AggregateTransformation$OPTIONS, collapse = " "), " (given: ", type, ")", call. = FALSE)
          }
          aggr_cols <- names(self$aggregations[self$aggregations == type])
          aggr_cols <- unique(aggr_cols)
          private$get_code_for_aggregator(type, aggr_cols, name_in)
        })
      }
    },

    get_code_for_aggregator = function(type, aggr_cols, name_in) {
      if (type == "size") {
        fxn <- "length"
      } else {
        fxn <- type
      }

      if (self$tidyverse) {
        glue::glue(
          "{aggr_cols}_{type} = {fxn}({aggr_cols})"
        )
      } else {
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

    initialize = function(cols, aggregations, name_out = NULL, tidyverse = NULL) {
      if (length(cols) == 0) {
        stop("You must provide at least one column to group by", call. = FALSE)
      }
      if (length(aggregations) == 0) {
        stop("You must provide at least one aggregation", call. = FALSE)
      }
      super$initialize(name_out, tidyverse)
      private$.cols <- cols
      private$.aggregations <- aggregations
      invisible(self)
    },

    print = function() {
      super$print()
      cat0(
        "\n",
        "    Group by: ",
        paste(self$cols, collapse = ", "), "\n",
        "    Aggregate by: ",
        paste(names(self$aggregations), unname(self$aggregations), sep = "=", collapse = ", "), "\n"
      )
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
      shinyjs::useShinyjs(),
      shiny::singleton(tags$head(tags$style(
        ".aggregate_existing_ui .row {position: relative; margin-bottom: 3px;}
             .aggregate_existing_ui .row .remove-btn { cursor: pointer; position: absolute; left: 15px; right: 15px; top: 0; bottom: 0; align-items: center; justify-content: center; z-index: 1; background: #fbe9e9; font-weight: bold; display: none; }
             .aggregate_existing_ui .row:hover .remove-btn { display: flex !important; }"
      ))),
      fluidRow(
        column(3, shinyWidgets::pickerInput(ns("aggregate_cols"), "Group by", NULL, multiple = TRUE)),
        column(3, shinyWidgets::pickerInput(ns("aggregate_col_agg"), "Aggregate on", "")),
        column(3, shinyWidgets::pickerInput(ns("aggregate_aggregator"), "Aggregator", c("", unname(AggregateTransformation$OPTIONS)))),
        column(3, textInput(ns("aggregate_name"), "New name", "dfg"))
      ),
      fluidRow(
        column(6, offset = 3, uiOutput(ns("aggregate_existing_ui"), class = "aggregate_existing_ui"))
      ),
      shinyjs::hidden(textInput(ns("aggregate_existing"), "", "[]"))
    )
  },

  server = function(id, data = reactive(NULL), old_xform = reactive(NULL)) {
    moduleServer(
      id,
      function(input, output, session) {

        ns <- session$ns

        observeEvent(data(), {
          shinyWidgets::updatePickerInput(session, "aggregate_cols", choices = names(data()))
          shinyWidgets::updatePickerInput(session, "aggregate_col_agg", choices = c("", names(data())))
        })

        observeEvent(old_xform(), {
          if (inherits(old_xform(), AggregateTransformation$classname)) {
            aggs <- lapply(seq_along(old_xform()$aggregations), function(idx) {
              as.list(old_xform()$aggregations[idx])
            })
            shinyWidgets::updatePickerInput(session, "xform_type", selected = "aggregate")
            shinyWidgets::updatePickerInput(session, "aggregate_cols", selected = old_xform()$cols)
            updateTextInput(session, "aggregate_existing", value = as.character(jsonlite::toJSON(aggs)))
            updateTextInput(session, "aggregate_name", value = old_xform()$name_out)
          }
        })

        observeEvent(input$aggregate_name, {
          if (!is_valid_name(input$aggregate_name)) {
            updateTextInput(session, "aggregate_name", value = make.names(input$aggregate_name))
          }
        })

        validate <- reactive({
          length(input$aggregate_cols) > 0 && is_valid_name(input$aggregate_name) &&
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
          shinyWidgets::updatePickerInput(session, "aggregate_col_agg", selected = "")
          shinyWidgets::updatePickerInput(session, "aggregate_aggregator", selected = "")

          new_aggregation <- list(as.list(stats::setNames(input$aggregate_aggregator, input$aggregate_col_agg)))
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
