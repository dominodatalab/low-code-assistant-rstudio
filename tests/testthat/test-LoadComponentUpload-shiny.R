test_that("LoadComponentUpload module works", {
  app <- shinyApp(
    fluidPage(assistDomino:::LoadComponentUpload$shiny$ui("test")),
    function(input, output, session) {
      res <- assistDomino:::LoadComponentUpload$shiny$server("test")
      exportTestValues(
        name = res$name(),
        code = if (is.null(res$code())) NULL else gsub("\\(.*\\)", "(<file>)", res$code()),
        data = res$data(),
        error = res$error()
      )
    }
  )
  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "LoadComponentUpload")

  driver$upload_file("test-file" = system.file("tests_data", "load_data", "cars.csv", package = "assistDomino"))
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_true(is_shiny_visible(driver, "test-params-header"))
  expect_true(is_shiny_visible(driver, "test-params-sep"))
  expect_true(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$upload_file("test-file" = system.file("tests_data", "load_data", "cars.blahblah", package = "assistDomino"))
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$upload_file("test-file" = system.file("tests_data", "load_data", "cars.xlsx", package = "assistDomino"))
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_true(is_shiny_visible(driver, "test-params-col_names"))
  expect_true(is_shiny_visible(driver, "test-params-sheet"))
})
