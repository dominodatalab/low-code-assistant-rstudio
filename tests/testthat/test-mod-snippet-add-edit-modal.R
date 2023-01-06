library(shiny)

skip_on_cran()

test_that("snippet-add-edit-modal works", {
  app <- shinyApp(
    ui <- fluidPage(
      shinyjs::useShinyjs(),
      actionButton("show_add", "show add"),
      actionButton("show_edit", "show edit")
    ),
    server = function(input, output, session) {
      modal <- assistDomino:::snippet_add_edit_modal("test", reactive(getwd()))

      observeEvent(input$show_add, {
        modal$show("add", "a/b/c")
      })
      observeEvent(input$show_edit, {
        modal$show("edit", "a/b/c")
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "snippet-add-edit-modal")

  driver$click("show_add")
  driver$wait_for_js("!!document.getElementById('test-name')")
  expect_false(is_shiny_visible(driver, "test-edit"))
  expect_true(is_shiny_visible(driver, "test-add"))

  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$set_inputs("test-name" = "snippet")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('x <- 5')")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))
  driver$expect_values(output = TRUE, input = c("test-add", "test-contents", "test-edit", "test-name"))

  driver$set_inputs("test-name" = "snippet/s")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$set_inputs("test-name" = "mysnippet")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('')")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('y <- x')")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))

  driver$click("test-add")
})



