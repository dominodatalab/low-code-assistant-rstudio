
test_that("Transformation parameters error correctly", {


  expect_error(FilterTransformation$new("col", "<", 5), NA)
  expect_error(FilterTransformation$new(c("col1", "col2"), "<", 5))
  expect_error(FilterTransformation$new())
  expect_error(FilterTransformation$new("col", "<>", 5))
  expect_error(FilterTransformation$new("col", "<"))

  expect_error(MissingValuesTransformation$new(c("col1", "col2")))
})







test_that("FilterTransformation works", {
  dfin <- data.frame(
    a = c("A", "B", "C", "D"),
    b = c(1, 2, 2, 3)
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "==", "A", "character")),
    dfin[ dfin$a == "A", ]
  )
  expect_error(
    run_xform(dfin, FilterTransformation$new("a", "==", "B"))
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "!=", "A", "character")),
    dfin[ dfin$a != "A", ],
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", "==", 2)),
    dfin[ dfin$b == 2, ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", ">=", 2)),
    dfin[ dfin$b >= 2, ],
    ignore_attr = TRUE
  )
})

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

test_that("printing a transformation", {

})
