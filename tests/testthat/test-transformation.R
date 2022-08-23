test_that("Transformation name_out is correct", {
  expect_equal(DropTransformation$new("col")$name_out, "df")
  expect_equal(DropTransformation$new("col", "mydf")$name_out, "mydf")
})

test_that("Transformation parameters error correctly", {
  expect_error(DropTransformation$new("col"), NA)
  expect_error(DropTransformation$new(c("col1", "col2")), NA)
  expect_error(DropTransformation$new())
  expect_error(DropTransformation$new(list()))

  expect_error(SelectTransformation$new("col"), NA)
  expect_error(SelectTransformation$new(c("col1", "col2")), NA)
  expect_error(SelectTransformation$new())
  expect_error(SelectTransformation$new(list()))

  expect_error(FilterTransformation$new("col", "<", 5), NA)
  expect_error(FilterTransformation$new(c("col1", "col2"), "<", 5))
  expect_error(FilterTransformation$new())
  expect_error(FilterTransformation$new("col", "<>", 5))
  expect_error(FilterTransformation$new("col", "<"))
})

run_xform <- function(data, xform) {
  eval(parse(text = xform$get_code("data")))
}

test_that("DropTransformation works", {
  dfin <- data.frame(a = "a", b = "b", c = "c")

  expect_identical(
    run_xform(dfin, DropTransformation$new("a")),
    dfin[ , !(names(dfin) %in% "a")]
  )
  expect_identical(
    run_xform(dfin, DropTransformation$new(c("a", "b"))),
    dfin[ , !(names(dfin) %in% c("a", "b"))]
  )
})

test_that("SelectTransformation works", {
  dfin <- data.frame(a = "a", b = "b", c = "c")

  expect_identical(
    run_xform(dfin, SelectTransformation$new("a")),
    dfin["a"]
  )
  expect_identical(
    run_xform(dfin, SelectTransformation$new(c("a", "b"))),
    dfin[c("a", "b")]
  )
})

test_that("FilterTransformation works", {
  dfin <- data.frame(
    a = c("a", "a", "b", "c"),
    b = c(1, 2, 2, 3)
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "==", "a", "character")),
    dfin[ dfin$a == "a", ]
  )
  expect_error(
    run_xform(dfin, FilterTransformation$new("a", "==", "a"))
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "!=", "a", "character")),
    dfin[ dfin$a != "a", ]
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", "==", 2)),
    dfin[ dfin$b == 2, ]
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", ">=", 2)),
    dfin[ dfin$b >= 2, ]
  )
})
