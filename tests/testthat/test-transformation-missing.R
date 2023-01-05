test_that("MissingValuesTransformation works", {
  dfin <- data.frame(a = c(NA, "B", "C"), b = c("A", NA, "B"), c = c("A", "B", "C"))

  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new("a")),
    dfin[c(2, 3), ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new("b")),
    dfin[c(1, 3), ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new("c")),
    dfin
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new()),
    run_xform(dfin, MissingValuesTransformation$new(list()))
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new()),
    dfin[3, ],
    ignore_attr = TRUE
  )
})

test_that("MissingValuesTransformation parameters error correctly", {
  expect_error(MissingValuesTransformation$new(c("col1", "col2")))
})

test_that("MissingValuesTransformation printing", {
  xform <- MissingValuesTransformation$new("var1")
  expect_output(xform$print(), "<MissingValuesTransformation>.*df.*base.*var1")
  xform <- MissingValuesTransformation$new()
  expect_output(xform$print(), "<MissingValuesTransformation>.*df.*base.*Any")
})
