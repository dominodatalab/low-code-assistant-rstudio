Transformation <- R6::R6Class(
  "Transformation",
  cloneable = FALSE,

  private = list(
    .name_out = NULL,
    .tidyverse = NULL
  ),

  active = list(
    name_out = function() {
      private$.name_out
    },

    tidyverse = function() {
      private$.tidyverse
    }
  ),

  public = list(
    initialize = function(name_out = NULL, tidyverse = NULL) {
      private$.name_out <- name_out %||% "df"
      self$use_tidyverse(tidyverse %||% TRUE)
      invisible(self)
    },

    use_tidyverse = function(use) {
      if (!is.null(use)) {
        private$.tidyverse <- use
      }
      invisible(self)
    },

    print = function() {
      cat0(
        "<", class(self)[1], "> (", self$name_out, " | ",
        if (private$.tidyverse) "{tidyverse}" else "base R",
        ") "
      )
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
