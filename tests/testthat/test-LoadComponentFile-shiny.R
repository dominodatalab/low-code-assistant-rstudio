library(shiny)

skip_on_cran()

test_that("LoadComponentFile load_file_params shiny module works", {
  app <- shinyApp(
    ui = fluidPage(
      selectInput("data", "data", c("cars.csv", "cars.xlsx", "cars.sav")),
      assistDomino:::load_file_params_ui("test")
    ),
    server = function(input, output, session) {
      file <- reactive({
        system.file("tests_data", "load_data", input$data, package = "assistDomino")
      })
      params <- assistDomino:::load_file_params_server("test", file)
      exportTestValues(
        params = params()
      )
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "load_file_params")

  driver$wait_for_idle()
  driver$expect_values()
  expect_true(is_shiny_visible(driver, "test-header"))
  expect_true(is_shiny_visible(driver, "test-sep"))
  expect_true(is_shiny_visible(driver, "test-dec"))
  expect_false(is_shiny_visible(driver, "test-col_names"))
  expect_false(is_shiny_visible(driver, "test-sheet"))

  driver$set_inputs("test-sep" = ";")
  driver$wait_for_idle()
  driver$expect_values()

  driver$set_inputs("test-header" = FALSE, "test-sep" = "")
  driver$wait_for_idle()
  driver$expect_values()

  driver$set_inputs("test-header" = TRUE, "test-sep" = ".", "test-dec" = ",")
  driver$wait_for_idle()
  driver$expect_values()

  driver$set_inputs("test-sep" = ",", "test-dec" = ".")
  driver$wait_for_idle()
  driver$expect_values()

  driver$set_inputs(data = "cars.xlsx")
  driver$wait_for_idle()
  driver$expect_values()
  expect_false(is_shiny_visible(driver, "test-header"))
  expect_false(is_shiny_visible(driver, "test-sep"))
  expect_false(is_shiny_visible(driver, "test-dec"))
  expect_true(is_shiny_visible(driver, "test-col_names"))
  expect_true(is_shiny_visible(driver, "test-sheet"))

  driver$set_inputs("test-col_names" = FALSE, "test-sheet" = 4)
  driver$wait_for_idle()
  driver$expect_values()

  driver$set_inputs("test-sheet" = 1)
  driver$wait_for_idle()
  driver$expect_values()

  driver$set_inputs(data = "cars.sav")
  driver$wait_for_idle()
  driver$expect_values()
  expect_false(is_shiny_visible(driver, "test-header"))
  expect_false(is_shiny_visible(driver, "test-sep"))
  expect_false(is_shiny_visible(driver, "test-dec"))
  expect_false(is_shiny_visible(driver, "test-col_names"))
  expect_false(is_shiny_visible(driver, "test-sheet"))
})
