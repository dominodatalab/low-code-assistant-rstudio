# Groupby and aggregate: Columns to group by, Column to aggregate, Aggregator (size, sum, mean, min, max), New data name

Transformation <- R6::R6Class(
  "Transformation",

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

    initialize = function(col = NULL, op = NULL, value = NULL, name_out = NULL) {
      super$initialize(name_out)
      self$col <- col
      self$op <- op
      self$value <- value
      invisible(self)
    },

    get_code = function(name_in) {
      glue::glue(
        '{self$name_out} <- ',
        '{name_in}',
        '[ {name_in}[["{self$col}"]] {self$op} "{self$value}", ]'
      )
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

TransformationSequence <- R6::R6Class(
  "TransformationSequence",

  private = list(

    .ready = FALSE,
    .transformations = NULL, # list of Transformation objects
    .name_in = NULL,

    add_transformations = function(transformations) {
      if (is.null(transformations)) {
        return()
      }
      if (inherits(transformations, Transformation$classname)) {
        transformations <- list(transformations)
      }
      if (!is.list(transformations)) {
        stop("TransformationSequence$add() - Must be called with a list of <Transformation> objects", call. = FALSE)
      }

      name_in <- private$.name_in
      for (idx in seq_along(transformations)) {
        transformation <- transformations[[idx]]
        if (!inherits(transformation, Transformation$classname)) {
          stop("TransformationSequence$add() - Must be called with a list of <Transformation> objects", call. = FALSE)
        }
      }
      private$.transformations <- transformations

      invisible(self)
    }

  ),

  public = list(

    result = NULL,  # result data from running the transformations
    error = NULL,   # error message
    error_chunk = NULL,  # chunk number that caused error

    initialize = function(transformations = NULL, name_in = NULL) {
      private$.ready <- FALSE
      if (is.null(name_in)) {
        stop("TransformationSequence$new(): name_in must be provided")
      }

      private$.name_in <- name_in
      private$add_transformations(transformations)
      invisible(self)
    },

    get_name_in = function() {
      private$.name_in
    },

    get_transformations = function() {
      private$.transformations
    },

    get_code = function() {
      chunks <- self$get_code_chunks()
      paste(chunks, collapse = "\n")
    },

    get_code_chunks = function() {
      name_in <- private$.name_in
      chunks <- list()
      for (transformation in private$.transformations) {
        chunks <- append(chunks, transformation$get_code(name_in))
        name_in <- transformation$name_out
      }
      chunks <- append(chunks, name_in)
      chunks
    },

    get_result = function() {
      if (!private$.ready) self$run()
      self$result
    },

    run = function(env = parent.frame()) {
      self$error_chunk <- NULL
      self$error <- NULL

      chunks <- self$get_code_chunks()
      result <- tryCatch({
        for (chunk_idx in seq_along(chunks)) {
          chunk <- chunks[[chunk_idx]]
          result <- eval(parse(text = chunk), envir = env)
        }
        result
      }, error = function(err) {
        self$error_chunk <- chunk_idx
        self$error <- err$message
        NULL
      })
      self$result <- result
      private$.ready <- TRUE
      invisible(self)
    },

    has_error = function() {
      if (!private$.ready) self$run()
      !is.null(self$error)
    },

    get_error = function() {
      if (!private$.ready) self$run()
      self$error
    },

    get_error_line = function() {
      if (!private$.ready) self$run()
      self$error_chunk
    }

  )
)

DatasetTransformation <- R6::R6Class(
  "DatasetTransformation",

  public = list(

    data = NULL,
    transformations = NULL,  # TransformationSequence object

    initialize = function(data, transformations = NULL) {
      self$data <- data
      self$transformations <- transformations
      invisible(self)
    },

    run = function(env = parent.frame()) {
      assign(self$transformations$get_name_in(), self$data, envir = env)
      self$transformations$run(env = env)
    }

  )
)
