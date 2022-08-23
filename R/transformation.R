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
        '[, !(names({name_in}) %in% c("{paste(self$cols, collapse = \'", "\')}"))]'
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
        value <- glue::glue('"{value}"')
      }

      glue::glue(
        '{self$name_out} <- ',
        '{name_in}',
        '[ {name_in}[["{self$col}"]] {self$op} {value}, ]'
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
