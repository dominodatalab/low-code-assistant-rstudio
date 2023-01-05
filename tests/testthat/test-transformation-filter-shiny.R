library(shiny)

skip_on_cran()

test_that("FilterTransformation shiny", {
  app <- shinyApp(
    fluidPage(
      selectInput("dataset", "dataset", c("mtcars", "iris")),
      assistDomino:::FilterTransformation$shiny$ui("test"),
      verbatimTextOutput("xform")
    ),
    function(input, output, session) {
      dataset <- reactive({
        get(input$dataset)
      })
      xform <- reactive({
        assistDomino:::FilterTransformation$new(names(dataset())[1], "==", dataset()[1, 1], name_out = paste0(input$dataset, "_df"))
      })

      res <- assistDomino:::FilterTransformation$shiny$server("test", dataset, xform)

      output$xform <- renderPrint({
        req(res$validate())
        res$xform()
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-filter")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-filter_col` = "hp", `test-filter_op` = ">=")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-filter_value` = 100)
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "iris")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-filter_name` = "iris_df@")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "mtcars")
  driver$wait_for_idle()
  driver$expect_values()
})
