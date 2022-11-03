# TransformationSequence is a data structure that holds an ordered list of
# transformations. It does not hold the data on which the transformations
# operate, only the transformations themselves and the name of the initial data.

TransformationSequence <- R6::R6Class(
  "TransformationSequence",
  cloneable = FALSE,

  private = list(

    .transformations = NULL,
    .name_in = NULL,

    set_transformations = function(transformations) {
      if (length(transformations) == 0) {
        private$.transformations <- list()
        return()
      }
      if (inherits(transformations, Transformation$classname)) {
        transformations <- list(transformations)
      }
      if (!is.list(transformations)) {
        stop("TransformationSequence: Must be provided a list of <Transformation> objects", call. = FALSE)
      }

      for (transformation in transformations) {
        if (!inherits(transformation, Transformation$classname)) {
          stop("TransformationSequence: Must be provided a list of <Transformation> objects", call. = FALSE)
        }
      }
      private$.transformations <- transformations
    }

  ),

  active = list(
    name_in = function() {
      private$.name_in
    },

    name_out = function() {
      if (self$size == 0) {
        private$.name_in
      } else {
        tail(self$transformations, 1)[[1]]$name_out
      }
    },

    transformations = function() {
      private$.transformations
    },

    size = function() {
      length(private$.transformations)
    }
  ),

  public = list(

    initialize = function(transformations = list(), name_in) {
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

    get_code = function(tidyverse = TRUE) {
      chunks <- self$get_code_chunks(tidyverse = tidyverse)
      paste(chunks, collapse = "\n")
    },

    get_code_chunks = function(tidyverse = TRUE) {
      name_in <- private$.name_in
      all_libraries <- c()
      chunks <- list()
      for (transformation in private$.transformations) {
        code <- transformation$get_code(name_in, tidyverse = tidyverse)
        lines <- strsplit(code, "\n")[[1]]
        libraries <- lines[grepl("^library(.+)$", lines)]
        all_libraries <- unique(c(all_libraries, libraries))
        code <- remove_duplicate_lines(code, libraries)
        chunks <- append(chunks, code)
        name_in <- transformation$name_out
      }
      chunks <- append(all_libraries, chunks)
      if (self$size == 0) {
        chunks <- append(chunks, name_in)
      }
      chunks
    },

    add = function(transformation) {
      self$insert(transformation, self$size)
    },

    insert = function(transformation, after) {
      if (!inherits(transformation, Transformation$classname)) {
        stop("add/insert: Must be provided a <Transformation> object", call. = FALSE)
      }
      if (after < 0 || after > self$size) {
        stop("insert: 'after' must be between 0 and the number of transformations", call. = FALSE)
      }
      new_xforms <- append(self$transformations, transformation, after = after)
      TransformationSequence$new(transformations = new_xforms, name_in = self$name_in)
    },

    remove = function(n) {
      if (n < 1 || n > self$size) {
        stop("remove: 'n' must be between 1 and the number of transformations", call. = FALSE)
      }
      new_xforms <- self$transformations[-n]
      TransformationSequence$new(transformations = new_xforms, name_in = self$name_in)
    },

    head = function(n) {
      if (n < 0 || n > self$size) {
        stop("head: 'n' must be between 1 and the number of transformations", call. = FALSE)
      }
      new_xforms <- head(self$transformations, n)
      TransformationSequence$new(transformations = new_xforms, name_in = self$name_in)
    },

    run = function(env = parent.frame(), tidyverse = TRUE) {
      chunks <- self$get_code_chunks(tidyverse = tidyverse)
      result <- tryCatch({
        temp_result <- get(self$name_in, envir = env)
        for (chunk_idx in seq_along(chunks)) {
          chunk <- chunks[[chunk_idx]]
          res <- eval(parse(text = chunk), envir = env)
          if (is.data.frame(res)) {
            temp_result <- res
          }
        }
        TransformationsResult$new(result = temp_result)
      }, error = function(err) {
        msg <- iconv(cli::ansi_strip(conditionMessage(err)), "latin1", "ASCII", sub = "")
        TransformationsResult$new(result = temp_result, error = msg, error_line = chunk, error_line_num = chunk_idx)
      })
      result
    }

  )
)
