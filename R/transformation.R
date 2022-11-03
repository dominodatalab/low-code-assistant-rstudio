Transformation <- R6::R6Class(
  "Transformation",
  cloneable = FALSE,

  private = list(
    .name_out = NULL,
    .tidyverse = FALSE
  ),

  active = list(
    name_out = function() {
      private$.name_out
    }
  ),

  public = list(
    initialize = function(name_out = NULL, tidyverse = NULL) {
      private$.name_out <- name_out %||% "df"
      self$use_tidyverse(tidyverse %||% TRUE)
      invisible(self)
    },

    use_tidyverse = function(x) {
      if (isTRUE(x) || isFALSE(x)) {
        private$.tidyverse <- x
        invisible(self)
      } else {
        stop("tidyverse must be either `TRUE` or `FALSE")
      }
    }
  )
)

ALL_XFORMS <- c(
  SelectTransformation,
  DropTransformation,
  FilterTransformation,
  MissingValuesTransformation,
  AggregateTransformation
)
