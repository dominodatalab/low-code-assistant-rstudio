test_that("AggregateTransformation works", {
  dfin <- data.frame(
    var1 = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    var2 = c(10, 20, 10, 30, 10, 40, 10, 50, 10, 20),
    var3 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    var4 = c(5, 5, 5, 5, 6, 6, 6, 6, 7, 7),
    var5 = c("a", "a", "a", "b","b", "b", "c", "c", "c", "a")
  )

  expect_error(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1")))
  )
  expect_error(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1"), tidyverse = TRUE))
  )

  expect_error(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "bad")))
  )
  expect_error(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "bad"), tidyverse = TRUE))
  )

  expect_error(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "min"))),
    NA
  )
  expect_error(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "min"), tidyverse = TRUE)),
    NA
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "sum"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var1_sum = c(2, 4, 6, 8, 10)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "sum"), tidyverse = TRUE)),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var1_sum = c(2, 4, 6, 8, 10)
    ),
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var2" = "size"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var2_size = c(2L, 2L, 2L, 2L, 2L)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var2" = "size"), tidyverse = TRUE)),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var2_size = c(2L, 2L, 2L, 2L, 2L)
    ),
    ignore_attr = TRUE
  )

  suppressWarnings(expect_warning(
    run_xform(dfin, AggregateTransformation$new("var1", c("var5" = "mean")))
  ))
  suppressWarnings(expect_warning(
    run_xform(dfin, AggregateTransformation$new("var1", c("var5" = "mean"), tidyverse = TRUE))
  ))
  expect_warning(
    run_xform(dfin, AggregateTransformation$new("var1", c("var5" = "size"))),
    NA
  )
  expect_warning(
    run_xform(dfin, AggregateTransformation$new("var1", c("var5" = "size"), tidyverse = TRUE)),
    NA
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var5", c("var3" = "mean"))),
    data.frame(
      var5 = c("a", "b", "c"),
      var3_mean = c(4, 5, 8)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var5", c("var3" = "mean"), tidyverse = TRUE)),
    data.frame(
      var5 = c("a", "b", "c"),
      var3_mean = c(4, 5, 8)
    ),
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var2" = "sum"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var2_sum = c(30, 40, 50, 60, 30)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var2" = "sum")), tidyverse = TRUE),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var2_sum = c(30, 40, 50, 60, 30)
    )
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "sum"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var1_sum = c(2, 4, 6, 8, 10)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var1" = "sum"), tidyverse = TRUE)),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var1_sum = c(2, 4, 6, 8, 10)
    ),
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new(c("var1", "var4"), c("var2" = "mean"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var4 = c(5, 5, 6, 6, 7),
      var2_mean = c(15, 20, 25, 30, 15)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new(c("var1", "var4"), c("var2" = "mean"), tidyverse = TRUE)),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var4 = c(5, 5, 6, 6, 7),
      var2_mean = c(15, 20, 25, 30, 15)
    ),
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var2" = "sum", "var2" = "mean", "var3" = "sum"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var2_sum = c(30, 40, 50, 60, 30),
      var3_sum = c(3, 7, 11, 15, 19),
      var2_mean = c(15, 20, 25, 30, 15)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new("var1", c("var2" = "sum", "var2" = "mean", "var3" = "sum"), tidyverse = TRUE)),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var2_sum = c(30, 40, 50, 60, 30),
      var2_mean = c(15, 20, 25, 30, 15),
      var3_sum = c(3, 7, 11, 15, 19)
    ),
    ignore_attr = TRUE
  )

  expect_identical(
    run_xform(dfin, AggregateTransformation$new(c("var1", "var4"), c("var3" = "max", "var2" = "min"))),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var4 = c(5, 5, 6, 6, 7),
      var3_max = c(2, 4, 6, 8, 10),
      var2_min = c(10, 10, 10, 10, 10)
    )
  )
  expect_identical(
    run_xform(dfin, AggregateTransformation$new(c("var1", "var4"), c("var3" = "max", "var2" = "min")), tidyverse = TRUE),
    data.frame(
      var1 = c(1, 2, 3, 4, 5),
      var4 = c(5, 5, 6, 6, 7),
      var3_max = c(2, 4, 6, 8, 10),
      var2_min = c(10, 10, 10, 10, 10)
    )
  )
})

test_that("AggregateTransormation parameters error correctly", {
  expect_error(AggregateTransformation$new())
  expect_error(AggregateTransformation$new(cols = list()))
  expect_error(AggregateTransformation$new(aggregations = list()))
  expect_error(AggregateTransformation$new(c("var1", "var2")))
  expect_error(AggregateTransformation$new(aggregations = c("col1" = "sum", "col2" = "min")))
  expect_error(AggregateTransformation$new(c("var1", "var2"), aggregations = c("col1" = "sum", "col2" = "min")), NA)
})

test_that("AggregateTransformation printing", {
  xform <- AggregateTransformation$new(c("var1", "var2"), c("col1" = "sum", "col2" = "min"))
  expect_output(xform$print(), "<AggregateTransformation>.*df.*base.*var1, var2.*col1.*col2")
})
