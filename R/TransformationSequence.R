# TODO add tests

# TransformationSequence is a data structure that holds an ordered list of
# transformations. It does not hold the data on which the transformations
# operate, only the transformations themselves and the name of the initial data.

TransformationSequence <- R6::R6Class(
  "TransformationSequence",
  cloneable = FALSE,

  private = list(

    .transformations = list(),
    .name_in = NULL,

    set_transformations = function(transformations) {
      if (is.null(transformations)) {
        return()
      }
      if (inherits(transformations, Transformation$classname)) {
        transformations <- list(transformations)
      }
      if (!is.list(transformations)) {
        stop("TransformationSequence: Must be called with a list of <Transformation> objects", call. = FALSE)
      }

      for (idx in seq_along(transformations)) {
        transformation <- transformations[[idx]]
        if (!inherits(transformation, Transformation$classname)) {
          stop("TransformationSequence: Must be called with a list of <Transformation> objects", call. = FALSE)
        }
      }
      private$.transformations <- transformations
    }

  ),

  public = list(

    initialize = function(transformations = list(), name_in = NULL) {
      if (is.null(name_in)) {
        stop("TransformationSequence: name_in must be provided")
      }

      private$.name_in <- name_in
      private$set_transformations(transformations)
      invisible(self)
    },

    print = function() {
      cat0("<TransformationSequence> with ", length(private$.transformations), " transformations:\n")
      for (idx in seq_along(private$.transformations)) {
        cat0("    ", idx, ". ")
        print(private$.transformations[[idx]])
      }
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

    add_transformation = function(transformation) {
      if (!inherits(transformation, Transformation$classname)) {
        stop("add_transformation: Must be called with a <Transformation> objects", call. = FALSE)
      }
      new_xforms <- append(self$get_transformations(), transformation)
      TransformationSequence$new(transformations = new_xforms, name_in = self$get_name_in())
    },

    run = function(env = parent.frame()) {
      chunks <- self$get_code_chunks()
      result <- tryCatch({
        temp_result <- get(self$get_name_in(), envir = env)
        for (chunk_idx in seq_along(chunks)) {
          chunk <- chunks[[chunk_idx]]
          temp_result <- eval(parse(text = chunk), envir = env)
        }
        TransformationsResult$new(result = temp_result)
      }, error = function(err) {
        TransformationsResult$new(error = err$message, error_line = chunk, error_line_num = chunk_idx)
      })
      result
    }

  )
)
