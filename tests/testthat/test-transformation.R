test_that("Transformation name_out is correct", {
  expect_equal(Transformation$new()$name_out, "df")
  expect_equal(Transformation$new("mydf")$name_out, "mydf")
})

test_that("Transformation tidyverse is set correctly", {
  expect_false(Transformation$new()$tidyverse)
  expect_false(Transformation$new(tidyverse = NULL)$tidyverse)
  expect_true(Transformation$new(tidyverse = TRUE)$tidyverse)
  expect_false(Transformation$new(tidyverse = FALSE)$tidyverse)
})

test_that("Transformation use_tidyverse(NULL) is set correctly", {
  expect_false(Transformation$new()$use_tidyverse(NULL)$tidyverse)
  expect_false(Transformation$new(tidyverse = NULL)$use_tidyverse(NULL)$tidyverse)
  expect_true(Transformation$new(tidyverse = TRUE)$use_tidyverse(NULL)$tidyverse)
  expect_false(Transformation$new(tidyverse = FALSE)$use_tidyverse(NULL)$tidyverse)
})

test_that("Transformation use_tidyverse(FALSE) is set correctly", {
  expect_false(Transformation$new()$use_tidyverse(FALSE)$tidyverse)
  expect_false(Transformation$new(tidyverse = NULL)$use_tidyverse(FALSE)$tidyverse)
  expect_false(Transformation$new(tidyverse = TRUE)$use_tidyverse(FALSE)$tidyverse)
  expect_false(Transformation$new(tidyverse = FALSE)$use_tidyverse(FALSE)$tidyverse)
})

test_that("Transformation use_tidyverse(TRUE) is set correctly", {
  expect_true(Transformation$new()$use_tidyverse(TRUE)$tidyverse)
  expect_true(Transformation$new(tidyverse = NULL)$use_tidyverse(TRUE)$tidyverse)
  expect_true(Transformation$new(tidyverse = TRUE)$use_tidyverse(TRUE)$tidyverse)
  expect_true(Transformation$new(tidyverse = FALSE)$use_tidyverse(TRUE)$tidyverse)
})

test_that("Transformation multiple dependencies get added to the code", {
  TestTransformation <- R6::R6Class(
    "TestTransformation",
    inherit = Transformation,

    private = list(
      get_dependencies = function() {
        c("dep1", "dep2")
      },

      get_full_code = function(name_in) {
        "1"
      }
    )
  )

  code <- TestTransformation$new()$get_code()
  expect_match(code, "library(dep1)", fixed = TRUE)
  expect_match(code, "library(dep2)", fixed = TRUE)
})
