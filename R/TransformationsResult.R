# TransformationsResult stores the result and, if applicable, error information
# from running a TransformationSequence. This information is not stored directly
# in a TransformationSequence so that the resulting data, which can potentially
# be a large object, won't be stored and take up a lot of unnecessary memory.
# This could happen if you have a list of TransformationSequence objects, with
# each one storing its own result.

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
    },

    print = function() {
      cat("<TransformationsResult>\n")
      if (!is.null(self$error)) {
        cat("Contains an error:", self$error, "\n")
      }
      if (!is.null(self$error_line) || !is.null(self$error_line_num)) {
        cat("Error from")
        if (!is.null(self$error_line_num)) {
          cat(" line", self$error_line_num)
        }
        if (!is.null(self$error_line)) {
          cat(":", self$error_line)
        }
      }
      if (!is.null(self$result)) {
        cat("\nResult: ")
        if (is.atomic(self$result)) {
          cat(self$result)
        } else if (is.data.frame(self$result)) {
          str(self$result)
        } else {
          print(self$result)
        }
      }
    }
  )
)
