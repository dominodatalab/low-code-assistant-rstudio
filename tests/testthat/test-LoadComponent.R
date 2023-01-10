test_that("LoadComponent works", {
  LoadComponentTest <- R6::R6Class(
    "LoadComponentTest",
    inherit = LoadComponent,

    private = list(
      fail = "",

      get_name = function() {
        if (private$fail == "name") {
          stop("bad name")
        }
        "good name"
      },
      get_code = function() {
        if (private$fail == "code") {
          stop("bad code")
        }
        "some code"
      },
      get_data = function() {
        if (private$fail == "data") {
          stop("bad data")
        }
        mtcars
      }
    ),

    public = list(
      initialize = function(fail = "") {
        private$fail <- fail
        private$run()
        invisible(self)
      }
    )
  )

  successful <- LoadComponentTest$new()
  expect_false(is.null(successful$code))
  expect_false(is.null(successful$name))
  expect_false(is.null(successful$data))
  expect_null(successful$error)
  expect_output(successful$print(), "<LoadComponent>.*good name.*some code")

  badcode <- LoadComponentTest$new("code")
  expect_null(badcode$code)
  expect_null(badcode$name)
  expect_null(badcode$data)
  expect_match(badcode$error, "bad code")
  expect_output(badcode$print(), "<LoadComponent>.*Error.*bad code")

  badname <- LoadComponentTest$new("name")
  expect_null(badname$code)
  expect_null(badname$name)
  expect_null(badname$data)
  expect_match(badname$error, "bad name")
  expect_output(badname$print(), "<LoadComponent>.*Error.*bad name")

  baddata <- LoadComponentTest$new("data")
  expect_null(baddata$code)
  expect_null(baddata$name)
  expect_null(baddata$data)
  expect_match(baddata$error, "bad data")
  expect_output(baddata$print(), "<LoadComponent>.*Error.*bad data")
})
