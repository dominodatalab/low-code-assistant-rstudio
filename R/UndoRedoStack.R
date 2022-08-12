UndoRedoStack <- R6::R6Class(
  "UndoRedoStack",
  cloneable = FALSE,

  private = list(

    .type = NULL,
    .undo_stack = list(),
    .redo_stack = list(),
    .current = NULL

  ),

  active = list(

    value = function() {
      private$.current
    },

    undo_size = function() {
      length(private$.undo_stack)
    },

    redo_size = function() {
      length(private$.redo_stack)
    }

  ),

  public = list(

    initialize = function(type = NULL) {
      private$.type <- type
    },

    undo = function() {
      if (self$undo_size < 1) {
        stop("undo: There is nothing to undo", call. = FALSE)
      }
      private$.redo_stack <- append(private$.redo_stack, private$.current)
      private$.current <- tail(private$.undo_stack, 1)[[1]]
      private$.undo_stack <- head(private$.undo_stack, -1)
      invisible(self)
    },

    redo = function() {
      if (self$redo_size < 1) {
        stop("redo: There is nothing to redo", call. = FALSE)
      }
      private$.undo_stack <- append(private$.undo_stack, private$.current)
      private$.current <- tail(private$.redo_stack, 1)[[1]]
      private$.redo_stack <- head(private$.redo_stack, -1)
      invisible(self)
    },

    add = function(item) {
      if (!is.null(private$.type) && !inherits(item, private$.type)) {
        stop("add: The provided item must be of type '", private$.type, "'", call. = FALSE)
      }

      if (is.null(private$.current)) {
        private$.undo_stack <- list()
      } else {
        private$.undo_stack <- append(private$.undo_stack, private$.current)
      }
      private$.current <- item
      private$.redo_stack <- list()
      invisible(self)
    }

  )
)
