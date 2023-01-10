test_that("DropTransformation works", {
  dfin <- data.frame(a = "A", b = "B", c = "C")

  expect_identical(
    run_xform(dfin, DropTransformation$new("a")),
    dfin[ , !(names(dfin) %in% "a")]
  )
  expect_identical(
    run_xform(dfin, DropTransformation$new("a", tidyverse = TRUE)),
    dfin[ , !(names(dfin) %in% "a")]
  )

  expect_identical(
    run_xform(dfin, DropTransformation$new(c("a", "b"))),
    dfin[ , !(names(dfin) %in% c("a", "b")), drop = FALSE]
  )
  expect_identical(
    run_xform(dfin, DropTransformation$new(c("a", "b"), tidyverse = TRUE)),
    dfin[ , !(names(dfin) %in% c("a", "b")), drop = FALSE]
  )
})

test_that("DropTransformation parameters error correctly", {
  expect_error(DropTransformation$new("col"), NA)
  expect_error(DropTransformation$new(c("col1", "col2")), NA)
  expect_error(DropTransformation$new())
  expect_error(DropTransformation$new(list()))
})

test_that("DropTransformation printing", {
  xform <- DropTransformation$new("var2", name_out = "mydf", tidyverse = TRUE)
  expect_output(xform$print(), "<DropTransformation>.*mydf.*tidyverse.*var2")
})
