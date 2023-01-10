test_that("LoadComponentProjectFile module works", {
  withr::local_envvar(list(DOMINO_WORKING_DIR = system.file("tests_data", "load_data", package = "assistDomino")))
  app <- shinyApp(
    fluidPage(assistDomino:::LoadComponentProjectFile$shiny$ui("test")),
    function(input, output, session) {
      res <- assistDomino:::LoadComponentProjectFile$shiny$server("test")
      exportTestValues(
        name = res$name(),
        code = if (is.null(res$code())) NULL else gsub("\\(.*\\)", "(<file>)", res$code()),
        data = res$data(),
        error = res$error()
      )
    }
  )
  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "LoadComponentProjectFile")

  driver$set_inputs(
    `test-filebrowser-file_clicked` = file.path(get_user_project_dir(), "cars.csv"),
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_true(is_shiny_visible(driver, "test-params-header"))
  expect_true(is_shiny_visible(driver, "test-params-sep"))
  expect_true(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs(
    `test-filebrowser-file_clicked` = file.path(get_user_project_dir(), "cars.csv"),
    allow_no_input_binding_ = TRUE, priority_ = "event",
  )
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs(
    `test-filebrowser-file_clicked` = file.path(get_user_project_dir(), "cars.xlsx"),
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_true(is_shiny_visible(driver, "test-params-col_names"))
  expect_true(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs("test-params-sheet" = 2)
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)

  driver$set_inputs("test-params-col_names" = FALSE)
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)

  driver$set_inputs(
    `test-filebrowser-file_clicked` = file.path(get_user_project_dir(), "cars_wrong_format.sas7bdat"),
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  expect_null(driver$get_value(export = "data"))
  expect_null(driver$get_value(export = "name"))
  expect_match(driver$get_value(export = "error"), "Could not load data")
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs(
    `test-filebrowser-file_clicked` = file.path(get_user_project_dir(), "cars_semicolon.txt"),
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_true(is_shiny_visible(driver, "test-params-header"))
  expect_true(is_shiny_visible(driver, "test-params-sep"))
  expect_true(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))

  driver$set_inputs("test-params-sep" = ";")
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)

  driver$set_inputs(
    `test-filebrowser-file_clicked` = file.path(get_user_project_dir(), "cars.sav"),
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )
  driver$wait_for_idle()
  driver$expect_values(export = TRUE)
  expect_false(is_shiny_visible(driver, "test-params-header"))
  expect_false(is_shiny_visible(driver, "test-params-sep"))
  expect_false(is_shiny_visible(driver, "test-params-dec"))
  expect_false(is_shiny_visible(driver, "test-params-col_names"))
  expect_false(is_shiny_visible(driver, "test-params-sheet"))
})
