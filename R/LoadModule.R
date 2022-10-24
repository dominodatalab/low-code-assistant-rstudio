# A subclass must implement: private$get_name(), private$get_code(), private$get_data()
LoadModule <- R6::R6Class(
  "LoadModule",
  cloneable = FALSE,

  private = list(
    .name = NULL,
    .code = NULL,
    .data = NULL,
    .error = NULL,

    run = function() {
      tryCatch({
        private$.name <- private$get_name()
        private$.code <- private$get_code()
        private$.data <- private$get_data()
        private$.error <- NULL
      }, error = function(err) {
        private$.name <- private$.code <- private$.data <- NULL
        private$.error <- glue::glue("Could not load data ({ err$message })")
      })
    }
  ),

  active = list(
    name = function() private$.name,
    code = function() private$.code,
    data = function() private$.data,
    error = function() private$.error
  ),

  public = list(
    initialize = function() {
      invisible(self)
    },

    print = function() {
      if (!is.null(private$.error)) {
        cat(glue::glue("<LoadModule> Error: { private$.error }\n"))
      } else {
        cat("<LoadModule>\n")
        cat0("Name: ", private$.name, "\n")
        cat0("Code: ", private$.code, "\n")
        str(private$.data)
      }
    }
  )
)
