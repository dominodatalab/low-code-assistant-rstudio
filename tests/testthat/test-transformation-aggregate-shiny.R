library(shiny)

skip_on_cran()

test_that("AggregateTransformation shiny", {
  app <- shinyApp(
    fluidPage(
      selectInput("dataset", "dataset", c("mtcars", "iris")),
      assistDomino:::AggregateTransformation$shiny$ui("test"),
      verbatimTextOutput("xform")
    ),
    function(input, output, session) {
      dataset <- reactive({
        get(input$dataset)
      })
      xform <- reactive({
        if (input$dataset == "iris") {
          assistDomino:::AggregateTransformation$new("Species", c(Sepal.Length = "sum", Sepal.Width = "mean"), name_out = paste0(input$dataset, "_df"))
        } else {
          assistDomino:::AggregateTransformation$new("cyl", c(mpg = "max"), name_out = paste0(input$dataset, "_df"))
        }
      })

      res <- assistDomino:::AggregateTransformation$shiny$server("test", dataset, xform)

      output$xform <- renderPrint({
        req(res$validate())
        res$xform()
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "transformation-aggregate")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_cols` = c("cyl", "am"))
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_col_agg` = "mpg")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_aggregator` = "sum")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_col_agg` = "drat")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_aggregator` = "sum")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_aggregator` = "sum")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_col_agg` = "mpg")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_remove` = 3, allow_no_input_binding_ = TRUE, priority_ = "event")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_remove` = 1, allow_no_input_binding_ = TRUE, priority_ = "event")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_remove` = 1, allow_no_input_binding_ = TRUE, priority_ = "event")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(dataset = "iris")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_name` = "iris_df@")
  driver$wait_for_idle()
  driver$expect_values()
  driver$set_inputs(`test-aggregate_cols` = "Species")
  driver$wait_for_idle()
  driver$expect_values()
})
