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
