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

test_that("Transformation parameters error correctly", {
  expect_error(DropTransformation$new("col"), NA)
  expect_error(DropTransformation$new(c("col1", "col2")), NA)
  expect_error(DropTransformation$new())
  expect_error(DropTransformation$new(list()))

  expect_error(SelectTransformation$new("col"), NA)
  expect_error(SelectTransformation$new(c("col1", "col2")), NA)
  expect_error(SelectTransformation$new())
  expect_error(SelectTransformation$new(list()))

  expect_error(FilterTransformation$new("col", "<", 5), NA)
  expect_error(FilterTransformation$new(c("col1", "col2"), "<", 5))
  expect_error(FilterTransformation$new())
  expect_error(FilterTransformation$new("col", "<>", 5))
  expect_error(FilterTransformation$new("col", "<"))

  expect_error(MissingValuesTransformation$new(c("col1", "col2")))
})

run_xform <- function(data, xform, tidyverse = TRUE) {
  (eval(parse(text = xform$get_code("data"))))
}

test_that("DropTransformation works", {
  dfin <- data.frame(a = "A", b = "B", c = "C")

  expect_identical(
    run_xform(dfin, DropTransformation$new("a")),
    dfin[ , !(names(dfin) %in% "a")]
  )
  expect_identical(
    run_xform(dfin, DropTransformation$new(c("a", "b"))),
    dfin[ , !(names(dfin) %in% c("a", "b")), drop = FALSE]
  )
})

test_that("SelectTransformation works", {
  dfin <- data.frame(a = "A", b = "B", c = "C")

  expect_identical(
    run_xform(dfin, SelectTransformation$new("a")),
    dfin["a"]
  )
  expect_identical(
    run_xform(dfin, SelectTransformation$new(c("a", "b"))),
    dfin[c("a", "b")]
  )
})

test_that("FilterTransformation works", {
  dfin <- data.frame(
    a = c("A", "B", "C", "D"),
    b = c(1, 2, 2, 3)
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "==", "A", "character")),
    dfin[ dfin$a == "A", ]
  )
  expect_error(
    run_xform(dfin, FilterTransformation$new("a", "==", "B"))
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "!=", "A", "character")),
    dfin[ dfin$a != "A", ],
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", "==", 2)),
    dfin[ dfin$b == 2, ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", ">=", 2)),
    dfin[ dfin$b >= 2, ],
    ignore_attr = TRUE
  )
})

test_that("MissingValuesTransformation works", {
  dfin <- data.frame(a = c(NA, "B", "C"), b = c("A", NA, "B"), c = c("A", "B", "C"))

  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new("a")),
    dfin[c(2, 3), ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new("b")),
    dfin[c(1, 3), ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new("c")),
    dfin
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new()),
    run_xform(dfin, MissingValuesTransformation$new(list()))
  )
  expect_identical(
    run_xform(dfin, MissingValuesTransformation$new()),
    dfin[3, ],
    ignore_attr = TRUE
  )
})

test_that("printing a transformation", {
  select_xform <- SelectTransformation$new("var1")
  drop_xform <- DropTransformation$new("var2", name_out = "mydf", tidyverse = TRUE)

  expect_output(select_xform$print(), "<SelectTransformation>.*df.*base.*var1")
  expect_output(drop_xform$print(), "<DropTransformation>.*mydf.*tidyverse.*var2")
})


test_that("multiple dependencies get added to the code", {
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
