# Groupby and aggregate: Columns to group by, Column to aggregate, Aggregator (size, sum, mean, min, max), New data name

Transformation <- R6::R6Class(
  "Transformation",

  private = list(
    .name_in = NULL,
    .name_out = NULL,
    .error = NULL
  ),

  public = list(

    initialize = function(name_out) {
      self$set_name_out(name_out %||% "df")
      invisible(self)
    },

    get_name_in = function() {
      if (is.null(private$.name_in)) {
        stop("Transformation name_in has not been set", call. = FALSE)
      }
      private$.name_in
    },

    set_name_in = function(name) {
      private$.name_in <- name
      invisible(self)
    },

    get_name_out = function() {
      private$.name_out
    },

    set_name_out = function(name) {
      private$.name_out <- name
      invisible(self)
    },

    get_code = function() {
      code <- private$get_partial_code()
      code <- glue::glue("{self$get_name_out()} <- {code}")
      code
    },

    apply = function() {
      name_in <- self$get_name_in()
      parent_frame <- parent.frame(1)
      code <- self$get_code()
      new_df <- eval(parse(text = code), envir = parent_frame)
      new_df
    }

  )
)

DropTransformation <- R6::R6Class(
  "DropTransformation",
  inherit = Transformation,

  private = list(
    get_partial_code = function() {
      name_in <- self$get_name_in()
      glue::glue(
        '{name_in}',
        '[, !(names({name_in}) %in% c("{paste(self$cols, collapse = \'", "\')}"))]'
      )
    }
  ),

  public = list(

    cols = NULL,

    initialize = function(cols = c(), name_out = NULL) {
      super$initialize(name_out)
      self$cols <- cols
      invisible(self)
    }

  )
)

SelectTransformation <- R6::R6Class(
  "SelectTransformation",
  inherit = Transformation,

  private = list(
    get_partial_code = function() {
      name_in <- self$get_name_in()
      glue::glue(
        '{name_in}',
        '[c("{paste(self$cols, collapse = \'", "\')}")]'
      )
    }
  ),

  public = list(

    cols = NULL,

    initialize = function(cols = c(), name_out = NULL) {
      super$initialize(name_out)
      self$cols <- cols
      invisible(self)
    }

  )
)

FilterTransformation <- R6::R6Class(
  "FilterTransformation",
  inherit = Transformation,

  private = list(
    get_partial_code = function() {
      name_in <- self$get_name_in()
      glue::glue(
        '{name_in}',
        '[ {name_in}[["{self$col}"]] {self$op} "{self$value}", ]'
      )
    }
  ),

  public = list(

    col = NULL,
    op = NULL,
    value = NULL,

    initialize = function(col = NULL, op = NULL, value = NULL, name_out = NULL) {
      super$initialize(name_out)
      self$col <- col
      self$op <- op
      self$value <- value
      invisible(self)
    }

  )
)

FilterTransformation$TRANSFORMATIONS <- list(
  LESS_THAN    = "<",
  GREATER_THAN = ">",
  LESS_EQ      = "<=",
  GREATER_EQ   = ">=",
  EQUALS       = "==",
  NOT_EQUALS   = "!="
)


##########

df <- mtcars
tr <- DropTransformation$new(c("disp", "hp"), "dfnew2")$
  set_name_in("df")
tr$apply()

se <- SelectTransformation$new(c("wt", "am", "hp"), "df3")

fi <- FilterTransformation$
  new("gear", FilterTransformation$TRANSFORMATIONS$GREATER_EQ, 4)
