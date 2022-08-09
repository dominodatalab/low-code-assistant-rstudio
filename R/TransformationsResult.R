# TransformationsResult stores the result and, if applicable, error information
# from running a TransformationSequence. This information is not stored directly
# in a TransformationSequence so that the resulting data, which can potentially
# be a large object, won't be stored and take up a lot of unnecessary memory.
# This could happen if you have a list of TransformationSequence objects, with
# each one storing its own result.

# TODO add $print()
# TODO add tests

TransformationsResult <- R6::R6Class(
  "TransformationsResult",
  cloneable = FALSE,

  private = list(
    .result = NULL,
    .error = NULL,
    .error_line = NULL,
    .error_line_num = NULL
  ),

  active = list(
    result = function() {
      private$.result
    },
    error = function() {
      private$.error
    },
    error_line = function() {
      private$.error_line
    },
    error_line_num = function() {
      private$.error_line_num
    }
  ),

  public = list(
    initialize = function(result = NULL, error = NULL, error_line = NULL, error_line_num = NULL) {
      private$.result <- result
      private$.error <- error
      private$.error_line <- error_line
      private$.error_line_num <- error_line_num
      invisible(self)
    }
  )
)
