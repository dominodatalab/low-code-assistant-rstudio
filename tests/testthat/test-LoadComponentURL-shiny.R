test_that("LoadComponentURL module works", {
  app <- shinyApp(
    fluidPage(assistDomino:::LoadComponentURL$shiny$ui("test")),
    function(input, output, session) {
      res <- assistDomino:::LoadComponentURL$shiny$server("test")
      exportTestValues(
        name = res$name(),
        code = if (is.null(res$code())) NULL else gsub("\\(.*\\)", "(<file>)", res$code()),
        data = res$data(),
        error = res$error()
      )
    }
  )
  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "LoadComponentURL")

  driver$set_inputs("test-url" = "https://raw.githubusercontent.com/dominodatalab/low-code-assistant-rstudio/master/inst/sample_data/penguins.csv")
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_true(is_shiny_visible(driver, "test-params-header"))
  expect_true(is_shiny_visible(driver, "test-params-sep"))
  expect_true(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs("test-url" = "https://raw.githubusercontent.com/dominodatalab/low-code-assistant-rstudio/master/inst/sample_data/penguins")
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs("test-url" = "https://raw.githubusercontent.com/dominodatalab/low-code-assistant-rstudio/master/inst/sample_data/penguins.blabla")
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs("test-url" = "")
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs("test-url" = "https://raw.githubusercontent.com/dominodatalab/low-code-assistant-rstudio/master/inst/sample_data/penguins.csv")
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_true(is_shiny_visible(driver, "test-params-header"))
  expect_true(is_shiny_visible(driver, "test-params-sep"))
  expect_true(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))
})
