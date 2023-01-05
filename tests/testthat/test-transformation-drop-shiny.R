library(shiny)

test_that("DropTransformation shiny ui", {
  expect_snapshot(DropTransformation$shiny$ui("test"))
})

test_that("DropTransformation shiny server", {
  testServer(DropTransformation$shiny$server, {
    expect_false(session$returned$validate())
    expect_error(session$returned$xform())

    session$setInputs(drop_cols = "c1")
    expect_false(session$returned$validate())

    session$setInputs(drop_name = "bad name")
    expect_false(session$returned$validate())

    session$setInputs(drop_name = "good.name")
    expect_true(session$returned$validate())

    session$setInputs(drop_cols = c())
    expect_false(session$returned$validate())

    session$setInputs(drop_cols = c("c1", "c2"))
    expect_true(session$returned$validate())
    expect_identical(session$returned$xform(), DropTransformation$new(c("c1", "c2"), "good.name"))
  })
})

testthat::skip_on_cran()

test_that("DropTransformation shiny simple", {
  app <- shinyApp(
    fluidPage(assistDomino:::DropTransformation$shiny$ui("test")),
    function(input, output, session) {
      assistDomino:::DropTransformation$shiny$server(
        "test",
        reactive(mtcars),
        reactive(assistDomino:::DropTransformation$new("cyl", "mydf"))
      )
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-drop-simple")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-drop_name` = "bad name")
  driver$set_inputs(`test-drop_cols` = c("mpg", "wt"))
  driver$expect_values()
})

test_that("DropTransformation shiny dynamic", {
  app <- shinyApp(
    fluidPage(
      selectInput("dataset", "dataset", c("mtcars", "iris")),
      assistDomino:::DropTransformation$shiny$ui("test"),
      verbatimTextOutput("xform")
    ),
    function(input, output, session) {
      dataset <- reactive({
        get(input$dataset)
      })
      xform <- reactive({
        assistDomino:::DropTransformation$new(names(dataset())[1:2], paste0(input$dataset, "_df"))
      })

      res <- assistDomino:::DropTransformation$shiny$server("test", dataset, xform)

      output$xform <- renderPrint({
        req(res$validate())
        res$xform()
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-drop-dynamic")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "iris")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "mtcars")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-drop_cols` = c("hp", "qsec"), `test-drop_name` = "myname")
  driver$expect_values()
})
