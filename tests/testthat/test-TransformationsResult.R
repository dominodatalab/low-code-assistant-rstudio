test_that("TransformationsResult works", {
  expect_output(
    TransformationsResult$new(error = "bad error")$print(),
    "TransformationsResult.*error.*bad error"
  )

  expect_output(
    TransformationsResult$new(error = "bad error", error_line_num = 5)$print(),
    "TransformationsResult.*error.*bad error.*line 5"
  )

  expect_output(
    TransformationsResult$new(error = "bad error", error_line_num = 5, error_line = "x <- 1")$print(),
    "TransformationsResult.*error.*bad error.*line 5.*x <- 1"
  )

  expect_output(
    TransformationsResult$new(result = "success")$print(),
    "TransformationsResult.*Result:.*success"
  )

  expect_output(
    TransformationsResult$new(result = mtcars)$print(),
    "TransformationsResult.*Result.*data.frame"
  )

  expect_output(
    TransformationsResult$new(result = Transformation$new())$print(),
    "TransformationsResult.*Result.*Transformation"
  )

  expect_output(
    TransformationsResult$new(result = "failure", error = "some error")$print(),
    "TransformationsResult.*error.*some error.*Result.*failure"
  )
})
