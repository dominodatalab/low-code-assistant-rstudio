library(shiny)

test_that("SelectTransformation shiny ui", {
  expect_snapshot(SelectTransformation$shiny$ui("test"))
})

test_that("SelectTransformation shiny server", {
  testServer(SelectTransformation$shiny$server, {
    expect_false(session$returned$validate())
    expect_error(session$returned$xform())

    session$setInputs(select_cols = "c1")
    expect_false(session$returned$validate())

    session$setInputs(select_name = "bad name")
    expect_false(session$returned$validate())

    session$setInputs(select_name = "good.name")
    expect_true(session$returned$validate())

    session$setInputs(select_cols = c())
    expect_false(session$returned$validate())

    session$setInputs(select_cols = c("c1", "c2"))
    expect_true(session$returned$validate())
    expect_identical(session$returned$xform(), SelectTransformation$new(c("c1", "c2"), "good.name"))
  })
})

skip_on_cran()

test_that("SelectTransformation shiny simple", {
  app <- shinyApp(
    fluidPage(assistDomino:::SelectTransformation$shiny$ui("test")),
    function(input, output, session) {
      assistDomino:::SelectTransformation$shiny$server(
        "test",
        reactive(mtcars),
        reactive(assistDomino:::SelectTransformation$new("cyl", "mydf"))
      )
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-select-simple")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-select_name` = "bad name")
  driver$set_inputs(`test-select_cols` = c("mpg", "wt"))
  driver$expect_values()
})

test_that("SelectTransformation shiny dynamic", {
  app <- shinyApp(
    fluidPage(
      selectInput("dataset", "dataset", c("mtcars", "iris")),
      assistDomino:::SelectTransformation$shiny$ui("test"),
      verbatimTextOutput("xform")
    ),
    function(input, output, session) {
      dataset <- reactive({
        get(input$dataset)
      })
      xform <- reactive({
        assistDomino:::SelectTransformation$new(names(dataset())[1:2], paste0(input$dataset, "_df"))
      })

      res <- assistDomino:::SelectTransformation$shiny$server("test", dataset, xform)

      output$xform <- renderPrint({
        req(res$validate())
        res$xform()
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-select-dynamic")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "iris")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "mtcars")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-select_cols` = c("hp", "qsec"), `test-select_name` = "myname")
  driver$expect_values()
})
