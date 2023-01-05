test_that("SelectTransformation works", {
  dfin <- data.frame(a = "A", b = "B", c = "C")

  expect_identical(
    run_xform(dfin, SelectTransformation$new("a")),
    dfin["a"]
  )
  expect_identical(
    run_xform(dfin, SelectTransformation$new("a", tidyverse = TRUE)),
    dfin["a"]
  )

  expect_identical(
    run_xform(dfin, SelectTransformation$new(c("a", "b"))),
    dfin[c("a", "b")]
  )
  expect_identical(
    run_xform(dfin, SelectTransformation$new(c("a", "b"), tidyverse = TRUE)),
    dfin[c("a", "b")]
  )
})

test_that("SelectTransformation parameters error correctly", {
  expect_error(SelectTransformation$new("col"), NA)
  expect_error(SelectTransformation$new(c("col1", "col2")), NA)
  expect_error(SelectTransformation$new())
  expect_error(SelectTransformation$new(list()))
})

test_that("SelectTransformation printing", {
  xform <- SelectTransformation$new("var1")
  xform2 <- SelectTransformation$new("var1",tidyverse = TRUE)
  expect_output(xform$print(), "<SelectTransformation>.*df.*base.*var1")
  expect_output(xform2$print(), "<SelectTransformation>.*df.*tidyverse.*var1")
})
