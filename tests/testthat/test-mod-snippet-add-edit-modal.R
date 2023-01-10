library(shiny)

skip_on_cran()

test_that("snippet-add-edit-modal adding snippet works", {
  app <- shinyApp(
    ui <- fluidPage(
      shinyjs::useShinyjs(),
      actionButton("show_add", "show add")
    ),
    server = function(input, output, session) {
      modal <- assistDomino:::snippet_add_edit_modal("test", reactive(c("dir1", "dir2")))

      observeEvent(input$show_add, {
        modal$show("add", "a/b/c")
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "snippet-add-edit-modal add")
  withr::local_dir(driver$get_dir())

  dir.create("dir1")
  dir.create("dir2")

  # First run: add a snippet

  driver$click("show_add")
  driver$wait_for_js("!!document.getElementById('test-name')")
  expect_false(is_shiny_visible(driver, "test-edit"))
  expect_true(is_shiny_enabled(driver, "test-name"))
  expect_true(is_shiny_enabled(driver, "test-repo"))

  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$set_inputs("test-name" = "snippet")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('x <- 5')")
  driver$wait_for_value(input = "test-contents")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))
  driver$expect_values(output = TRUE, input = c("show_add", "test-add", "test-contents", "test-edit", "test-name", "test-repo"))

  driver$set_inputs("test-name" = "snippet/s")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$set_inputs("test-name" = "mysnippet")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('')")
  driver$wait_for_value(input = "test-contents")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-add"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('y <- x')")
  driver$wait_for_value(input = "test-contents")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))
  driver$expect_values(output = TRUE, input = c("show_add", "test-add", "test-contents", "test-edit", "test-name", "test-repo"))

  driver$click("test-add")
  driver$run_js("swalService.close()")
  driver$wait_for_js("!document.getElementById('test-name')")
  expect_false(is_shiny_exists(driver, "test-add"))

  expect_true(file.exists("dir1/snippets/a/b/c/mysnippet.R"))
  expect_equal("y <- x", readLines("dir1/snippets/a/b/c/mysnippet.R"))

  # Second run: add another snippet

  driver$click("show_add")
  driver$wait_for_js("!!document.getElementById('test-name')")
  expect_false(is_shiny_visible(driver, "test-edit"))

  driver$set_inputs("test-name" = "newsnippet", "test-repo" = "dir2")
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('y2 <- x2')")
  driver$wait_for_value(input = "test-contents")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-add"))
  driver$expect_values(output = TRUE, input = c("show_add", "test-add", "test-contents", "test-edit", "test-name", "test-repo"))

  driver$click("test-add")
  driver$run_js("swalService.close()")
  driver$wait_for_js("!document.getElementById('test-name')")
  expect_false(is_shiny_exists(driver, "test-add"))

  expect_true(file.exists("dir2/snippets/a/b/c/newsnippet.R"))
  expect_equal("y2 <- x2", readLines("dir2/snippets/a/b/c/newsnippet.R"))
})

test_that("snippet-add-edit-modal editing snippet works", {
  app <- shinyApp(
    ui <- fluidPage(
      shinyjs::useShinyjs(),
      actionButton("show_edit", "show edit")
    ),
    server = function(input, output, session) {
      modal <- assistDomino:::snippet_add_edit_modal("test", reactive(c("dir1", "dir2")))

      observeEvent(input$show_edit, {
        modal$show("edit", "a/b/c", "dir2/snippets/a/b/c/foo.R")
      })
    }
  )

  driver <- shinytest2::AppDriver$new(app, expect_values_screenshot_args = FALSE, name = "snippet-add-edit-modal edit")
  withr::local_dir(driver$get_dir())

  dir.create("dir2/snippets/a/b/c", recursive = TRUE)
  writeLines("foo <- bar", "dir2/snippets/a/b/c/foo.R")

  # First run

  driver$click("show_edit")
  driver$wait_for_js("!!document.getElementById('test-name')")
  driver$wait_for_value(input = "test-contents")
  expect_true(is_shiny_visible(driver, "test-edit"))
  expect_false(is_shiny_visible(driver, "test-add"))
  expect_false(is_shiny_enabled(driver, "test-name"))
  expect_false(is_shiny_enabled(driver, "test-repo"))

  expect_true(is_shiny_enabled(driver, "test-edit"))
  driver$expect_values(output = TRUE, input = c("show_edit", "test-add", "test-contents", "test-edit", "test-name", "test-repo"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('')")
  driver$wait_for_value(input = "test-contents")
  driver$wait_for_idle()
  expect_false(is_shiny_enabled(driver, "test-edit"))

  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('foo2 <- bar2')")
  driver$wait_for_value(input = "test-contents")
  driver$wait_for_idle()
  expect_true(is_shiny_enabled(driver, "test-edit"))

  driver$click("test-edit")
  driver$run_js("swalService.close()")
  driver$wait_for_js("!document.getElementById('test-name')")
  expect_false(is_shiny_exists(driver, "test-edit"))

  expect_equal("foo2 <- bar2", readLines("dir2/snippets/a/b/c/foo.R"))

  # Second run

  driver$click("show_edit")
  driver$wait_for_js("!!document.getElementById('test-name')")
  driver$wait_for_value(input = "test-contents")
  driver$expect_values(output = TRUE, input = c("show_edit", "test-add", "test-contents", "test-edit", "test-name", "test-repo"))
  driver$run_js("$(document.getElementById('test-contents')).data('aceEditor').setValue('foo3 <- bar3')")
  driver$wait_for_idle()
  driver$click("test-edit")
  driver$run_js("swalService.close()")
  driver$wait_for_js("!document.getElementById('test-name')")
  expect_equal("foo3 <- bar3", readLines("dir2/snippets/a/b/c/foo.R"))
})
