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
        stop("TransformationSequence - Must be called with a list of <Transformation> objects", call. = FALSE)
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
      stopifnot(private$.ready)
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
      stopifnot(private$.ready)
      !is.null(self$error)
    },

    get_error = function() {
      stopifnot(private$.ready)
      self$error
    },

    get_error_line = function() {
      stopifnot(private$.ready)
      self$error_chunk
    },

    add_transformation = function(transformation) {
      new_xforms <- append(self$get_transformations(), transformation)
      TransformationSequence$new(transformations = new_xforms, name_in = self$get_name_in())
    }

  )
)
