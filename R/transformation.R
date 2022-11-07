# A subclass must implement: private$get_full_code(name_in) and private$get_dependencies()
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
    },

    dependencies = function() {
      private$get_dependencies()
    }
  ),

  public = list(
    initialize = function(name_out = NULL, tidyverse = NULL) {
      private$.name_out <- name_out %||% "df"
      self$use_tidyverse(tidyverse %||% FALSE)
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
    },

    get_code = function(name_in, dependencies = TRUE) {
      code <- paste(self$name_out, "<-", private$get_full_code(name_in))
      if (dependencies && length(private$get_dependencies()) >= 1) {
        code <- paste0(
          paste(paste0("library(", private$get_dependencies(), ")\n"), collapse = ""),
          code
        )
      }
      code
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
