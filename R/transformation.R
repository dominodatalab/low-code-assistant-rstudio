Transformation <- R6::R6Class(
  "Transformation",
  cloneable = FALSE,

  private = list(
    .name_out = NULL
  ),

  active = list(
    name_out = function() {
      private$.name_out
    }
  ),

  public = list(
    initialize = function(name_out) {
      private$.name_out <- name_out %||% "df"
      invisible(self)
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
