test_that("LoadComponentDemo module works", {
  app <- shinyApp(
    fluidPage(assistDomino:::LoadComponentDemo$shiny$ui("test")),
    function(input, output, session) {
      res <- assistDomino:::LoadComponentDemo$shiny$server("test")
      exportTestValues(
        name = res$name(),
        code = res$code(),
        data = res$data(),
        error = res$error()
      )
    }
  )
  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "LoadComponentDemo")

  driver$set_inputs(
    `test-datasets-file_clicked` = "economics",
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = c("name", "code", "error"))
  expect_identical(driver$get_value(export = "data"), ggplot2::economics)

  driver$set_inputs(
    `test-datasets-file_clicked` = "iris",
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = c("name", "code", "error"))
  expect_identical(driver$get_value(export = "data"), iris)

  driver$set_inputs(
    `test-datasets-file_clicked` = "iris",
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)

  driver$set_inputs(
    `test-datasets-file_clicked` = "gapminder",
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = c("name", "code", "error"))
  expect_identical(driver$get_value(export = "data"), gapminder::gapminder)
})
