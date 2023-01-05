library(shiny)

skip_on_cran()

test_that("MissingValuesTransformation shiny", {
  app <- shinyApp(
    fluidPage(
      selectInput("dataset", "dataset", c("mtcars", "iris")),
      assistDomino:::MissingValuesTransformation$shiny$ui("test"),
      verbatimTextOutput("xform")
    ),
    function(input, output, session) {
      dataset <- reactive({
        get(input$dataset)
      })
      xform <- reactive({
        if (input$dataset == "iris") {
          assistDomino:::MissingValuesTransformation$new(name_out = paste0(input$dataset, "_df"))
        } else {
          assistDomino:::MissingValuesTransformation$new(names(dataset())[1], paste0(input$dataset, "_df"))
        }
      })

      res <- assistDomino:::MissingValuesTransformation$shiny$server("test", dataset, xform)

      output$xform <- renderPrint({
        req(res$validate())
        res$xform()
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-missing")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-missing_col` = "hp")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "iris")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-missing_name` = "iris_df@")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "mtcars")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-missing_type` = "all")
  driver$wait_for_idle()
  driver$expect_values()
})
