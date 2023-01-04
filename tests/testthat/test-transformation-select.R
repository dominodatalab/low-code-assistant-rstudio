test_that("SelectTransformation works", {
  dfin <- data.frame(a = "A", b = "B", c = "C")

  expect_identical(
    run_xform(dfin, SelectTransformation$new("a")),
    dfin["a"]
  )
  expect_identical(
    run_xform(dfin, SelectTransformation$new(c("a", "b"))),
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
  select_xform <- SelectTransformation$new("var1")
  select_xform2 <- SelectTransformation$new("var1",tidyverse = TRUE)
  expect_output(select_xform$print(), "<SelectTransformation>.*df.*base.*var1")
  expect_output(select_xform2$print(), "<SelectTransformation>.*df.*tidyverse.*var1")
})

test_that("SelectTransformation shiny ui", {
  expect_snapshot(SelectTransformation$shiny$ui("test"))
})

test_that("SelectTransformation shiny server", {
  shiny::testServer(SelectTransformation$shiny$server, {
    expect_false(session$returned$validate())
    expect_error(session$returned$xform())

    session$setInputs(select_cols = c("c1", "c2"))
    expect_false(session$returned$validate())

    session$setInputs(select_name = "bad name")
    expect_false(session$returned$validate())

    session$setInputs(select_name = "good.name")
    expect_true(session$returned$validate())
    expect_identical(session$returned$xform(), SelectTransformation$new(c("c1", "c2"), "good.name"))
  })
})

test_that("SelectTransformation shiny app", {
  # app <- shiny::shinyApp(
  #   shiny::fluidPage(SelectTransformation$shiny$ui("test")),
  #   function(input, output, session) SelectTransformation$shiny$server("test", shiny::reactive(mtcars), shiny::reactive(SelectTransformation$new("cyl", "mydf")))
  # )
  #TODO
})
