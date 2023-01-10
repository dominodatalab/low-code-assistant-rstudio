test_that("FilterTransformation works", {
  dfin <- data.frame(
    a = c("A", "B", "C", "D"),
    b = c(1, 2, 2, 3)
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "==", "A", "character")),
    dfin[ dfin$a == "A", ]
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "==", "A", "character", tidyverse = TRUE)),
    dfin[ dfin$a == "A", ]
  )

  expect_error(
    run_xform(dfin, FilterTransformation$new("a", "==", "B"))
  )
  expect_error(
    run_xform(dfin, FilterTransformation$new("a", "==", "B", tidyverse = TRUE))
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "!=", "A", "character")),
    dfin[ dfin$a != "A", ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("a", "!=", "A", "character", tidyverse = TRUE)),
    dfin[ dfin$a != "A", ],
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", "==", 2)),
    dfin[ dfin$b == 2, ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", "==", 2, tidyverse = TRUE)),
    dfin[ dfin$b == 2, ],
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", ">=", 2)),
    dfin[ dfin$b >= 2, ],
    ignore_attr = TRUE
  )
  expect_identical(
    run_xform(dfin, FilterTransformation$new("b", ">=", 2, tidyverse = TRUE)),
    dfin[ dfin$b >= 2, ],
    ignore_attr = TRUE
  )
})

test_that("FilterTransformation parameters error correctly", {
  expect_error(FilterTransformation$new("col", "<", 5), NA)
  expect_error(FilterTransformation$new(c("col1", "col2"), "<", 5))
  expect_error(FilterTransformation$new())
  expect_error(FilterTransformation$new("col", "<>", 5))
  expect_error(FilterTransformation$new("col", "<"))
})

test_that("FilterTransformation printing", {
  xform <- FilterTransformation$new("mycol", "==", 2, name_out = "myvar")
  expect_output(xform$print(), "<FilterTransformation>.*myvar.*base.*mycol == 2")
})
