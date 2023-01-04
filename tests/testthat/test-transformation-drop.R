test_that("DropTransformation works", {
  dfin <- data.frame(a = "A", b = "B", c = "C")

  expect_identical(
    run_xform(dfin, DropTransformation$new("a")),
    dfin[ , !(names(dfin) %in% "a")]
  )
  expect_identical(
    run_xform(dfin, DropTransformation$new(c("a", "b"))),
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
  drop_xform <- DropTransformation$new("var2", name_out = "mydf", tidyverse = TRUE)
  expect_output(drop_xform$print(), "<DropTransformation>.*mydf.*tidyverse.*var2")
})

test_that("DropTransformation shiny ui", {
  expect_snapshot(DropTransformation$shiny$ui("test"))
})

test_that("DropTransformation shiny server", {
  shiny::testServer(DropTransformation$shiny$server, {
    expect_false(session$returned$validate())
    expect_error(session$returned$xform())

    session$setInputs(drop_cols = c("c1", "c2"))
    expect_false(session$returned$validate())

    session$setInputs(drop_name = "bad name")
    expect_false(session$returned$validate())

    session$setInputs(drop_name = "good.name")
    expect_true(session$returned$validate())
    expect_identical(session$returned$xform(), DropTransformation$new(c("c1", "c2"), "good.name"))
  })
})
