# TODO move all public fields to be private
# TODO add $print()
# TODO add tests

Transformation <- R6::R6Class(
  "Transformation",
  cloneable = FALSE,

  public = list(
    name_out = NULL,

    initialize = function(name_out) {
      self$name_out <- name_out %||% "df"
      invisible(self)
    }
  )
)

DropTransformation <- R6::R6Class(
  "DropTransformation",
  inherit = Transformation,

  public = list(

    cols = NULL,

    initialize = function(cols = c(), name_out = NULL) {
      super$initialize(name_out)
      self$cols <- cols
      invisible(self)
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

  public = list(

    cols = NULL,

    initialize = function(cols = c(), name_out = NULL) {
      super$initialize(name_out)
      self$cols <- cols
      invisible(self)
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

  public = list(

    col = NULL,
    op = NULL,
    value = NULL,
    type = NULL,

    initialize = function(col = NULL, op = NULL, value = NULL, type = NULL, name_out = NULL) {
      super$initialize(name_out)
      self$col <- col
      self$op <- op
      self$value <- value
      self$type <- type
      invisible(self)
    },

    get_code = function(name_in) {
      value <- self$value
      if (!is.null(self$type) && self$type %in% c("character", "factor")) {
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
