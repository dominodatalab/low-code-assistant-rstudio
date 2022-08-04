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
