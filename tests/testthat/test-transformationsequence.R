get_xform_sel1 <- function() {
  SelectTransformation$new("col1")
}
get_xform_sel2 <- function() {
  SelectTransformation$new(c("col1", "col2"))
}
get_xform_drop1 <- function() {
  DropTransformation$new("col1")
}
get_xform_drop2 <- function() {
  DropTransformation$new(c("col1", "col2"))
}

test_that("TransformationSequence needs a name_in", {
  expect_error(TransformationSequence$new())
  expect_error(TransformationSequence$new(name_in = "df"), NA)
})

test_that("TransformationSequence has the correct name_out", {
  expect_identical(
    TransformationSequence$new(name_in = "somedf")$name_out,
    "somedf"
  )
  expect_identical(
    TransformationSequence$new(Transformation$new(name_out = "otherdf"), name_in = "somedf")$name_out,
    "otherdf"
  )
})

test_that("TransformationSequence sets the correct transformations", {
  expect_identical(
    TransformationSequence$new(name_in = "df")$transformations,
    list()
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", get_xform_sel1())$transformations,
    TransformationSequence$new(name_in = "df", list(get_xform_sel1()))$transformations
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", get_xform_sel1())$transformations,
    list( get_xform_sel1() )
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(get_xform_sel1(), get_xform_sel2(), get_xform_drop1()))$transformations,
    list(get_xform_sel1(), get_xform_sel2(), get_xform_drop1())
  )
})

test_that("TransformationSequence dependencies are correct", {
  expect_identical(
    TransformationSequence$new(name_in = "df")$dependencies,
    c()
  )
  expect_identical(
    TransformationSequence$new(get_xform_sel1()$use_tidyverse(FALSE), name_in = "df")$dependencies,
    c()
  )
  expect_identical(
    TransformationSequence$new(get_xform_sel1()$use_tidyverse(TRUE), name_in = "df")$dependencies,
    "dplyr"
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1()$use_tidyverse(TRUE),
      get_xform_sel1()$use_tidyverse(TRUE),
      MissingValuesTransformation$new(tidyverse = TRUE)
    ))$dependencies,
    c("dplyr", "tidyr")
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1()$use_tidyverse(TRUE),
      get_xform_sel1()$use_tidyverse(TRUE),
      MissingValuesTransformation$new(tidyverse = FALSE)
    ))$dependencies,
    "dplyr"
  )
})

test_that("TransformationSequence add is correct", {
  expect_error(
    TransformationSequence$new(name_in = "df")$add()
  )
  expect_error(
    TransformationSequence$new(name_in = "df")$add(5)
  )

  expect_identical(
    TransformationSequence$new(name_in = "df")$
      add(get_xform_sel1())$transformations,
    list(get_xform_sel1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", get_xform_drop1())$
      add(get_xform_sel1())$transformations,
    list(get_xform_drop1(), get_xform_sel1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", get_xform_drop1())
    $add(get_xform_sel1())$transformations,
    list(get_xform_drop1(), get_xform_sel1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(get_xform_drop1(), get_xform_drop2()))
    $add(get_xform_sel1())$transformations,
    list(get_xform_drop1(), get_xform_drop2(), get_xform_sel1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(get_xform_sel1(), get_xform_drop1()))
    $add(get_xform_drop2())$transformations,
    list(get_xform_sel1(), get_xform_drop1(), get_xform_drop2())
  )
})

test_that("TransformationSequence insert is correct", {
  expect_error(
    TransformationSequence$new(name_in = "df")$insert()
  )
  expect_error(
    TransformationSequence$new(name_in = "df")$insert(5, 0)
  )
  expect_error(
    TransformationSequence$new(name_in = "df")$insert(get_xform_sel1(), 1)
  )
  expect_error(
    TransformationSequence$new(name_in = "df")$insert(get_xform_sel1(), -1)
  )


  expect_identical(
    TransformationSequence$new(name_in = "df")$
      insert(get_xform_sel1(), 0)$transformations,
    list(get_xform_sel1())
  )

  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$
      insert(get_xform_drop2(), 0)$transformations,
    list(get_xform_drop2(), get_xform_sel1(), get_xform_sel2(), get_xform_drop1())
  )

  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$
      insert(get_xform_drop2(), 1)$transformations,
    list(get_xform_sel1(), get_xform_drop2(), get_xform_sel2(), get_xform_drop1())
  )

  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$
      insert(get_xform_drop2(), 2)$transformations,
    list(get_xform_sel1(), get_xform_sel2(), get_xform_drop2(), get_xform_drop1())
  )


  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$
      insert(get_xform_drop2(), 3)$transformations,
    list(get_xform_sel1(), get_xform_sel2(), get_xform_drop1(), get_xform_drop2())
  )

  expect_error(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$
      insert(get_xform_drop2(), 4)
  )
})

test_that("TransformationSequence remove is correct", {
  expect_error(
    TransformationSequence$new(name_in = "df")$remove()
  )
  expect_error(
    TransformationSequence$new(name_in = "df")$remove(0)
  )
  expect_error(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$remove(-1)
  )
  expect_error(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$remove(0)
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", get_xform_sel1())$remove(1),
    TransformationSequence$new(name_in = "df")
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$remove(1)$transformations,
    list(get_xform_sel2(), get_xform_drop1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$remove(2)$transformations,
    list(get_xform_sel1(), get_xform_drop1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$remove(3)$transformations,
    list(get_xform_sel1(), get_xform_sel2())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$remove(3)$remove(1)$transformations,
    list(get_xform_sel2())
  )
})

test_that("TransformationSequence head is correct", {
  expect_error(
    TransformationSequence$new(name_in = "df")$head()
  )
  expect_identical(
    TransformationSequence$new(name_in = "df")$head(0),
    TransformationSequence$new(name_in = "df")
  )
  expect_error(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$head(-1)
  )
  expect_error(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$head(4)
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$head(0),
    TransformationSequence$new(name_in = "df")
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$head(1)$transformations,
    list(get_xform_sel1())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$head(2)$transformations,
    list(get_xform_sel1(), get_xform_sel2())
  )
  expect_identical(
    TransformationSequence$new(name_in = "df", list(
      get_xform_sel1(), get_xform_sel2(), get_xform_drop1()
    ))$head(3)$transformations,
    list(get_xform_sel1(), get_xform_sel2(), get_xform_drop1())
  )
})

test_that("TransformationSequence tidyverse is set correctly", {
  expect_null(TransformationSequence$new(name_in = "df")$tidyverse)
  expect_null(TransformationSequence$new(name_in = "df", tidyverse = NULL)$tidyverse)
  expect_true(TransformationSequence$new(name_in = "df", tidyverse = TRUE)$tidyverse)
  expect_false(TransformationSequence$new(name_in = "df", tidyverse = FALSE)$tidyverse)
})

get_tidyverse_status <- function(xformseq) {
  sapply(xformseq$transformations, function(xform) xform$tidyverse)
}

test_that("TransformationSequence sets tidyverse correctly on a single transformation when not given tidyverse", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", Transformation$new())
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", Transformation$new(tidyverse = NULL))
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", Transformation$new(tidyverse = TRUE))
    ),
    TRUE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", Transformation$new(tidyverse = FALSE))
    ),
    FALSE
  )
})

test_that("TransformationSequence sets tidyverse correctly on a single transformation when given NULL tidyverse", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = NULL, Transformation$new())
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = NULL, Transformation$new(tidyverse = NULL))
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = NULL, Transformation$new(tidyverse = TRUE))
    ),
    TRUE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = NULL, Transformation$new(tidyverse = FALSE))
    ),
    FALSE
  )
})

test_that("TransformationSequence sets tidyverse correctly on a single transformation when given FALSE tidyverse", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = FALSE, Transformation$new())
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = FALSE, Transformation$new(tidyverse = NULL))
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = FALSE, Transformation$new(tidyverse = TRUE))
    ),
    FALSE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = FALSE, Transformation$new(tidyverse = FALSE))
    ),
    FALSE
  )
})

test_that("TransformationSequence sets tidyverse correctly on a single transformation when given TRUE tidyverse", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = TRUE, Transformation$new())
    ),
    TRUE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = TRUE, Transformation$new(tidyverse = NULL))
    ),
    TRUE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = TRUE, Transformation$new(tidyverse = TRUE))
    ),
    TRUE
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(name_in = "df", tidyverse = TRUE, Transformation$new(tidyverse = FALSE))
    ),
    TRUE
  )
})

test_that("TransformationSequence sets tidyverse correctly on init on multiple transformations that don't have tidyverse set", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(), Transformation$new(), Transformation$new())
      )
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = NULL,
        list(Transformation$new(), Transformation$new(), Transformation$new())
      )
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(), Transformation$new(), Transformation$new())
      )
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(), Transformation$new(), Transformation$new())
      )
    ),
    c(TRUE, TRUE, TRUE)
  )
})

test_that("TransformationSequence sets tidyverse correctly on init on multiple transformations that have tidyverse set", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )
    ),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = NULL,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )
    ),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )
    ),
    c(TRUE, TRUE, TRUE)
  )
})

test_that("TransformationSequence use_tidyverse() correctly sets tidyverse on transformations", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(NULL)
    ),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(TRUE)
    ),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(FALSE)
    ),
    c(FALSE, FALSE, FALSE)
  )

  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(NULL)$use_tidyverse(FALSE)
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(FALSE)$use_tidyverse(TRUE)
    ),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(TRUE)$use_tidyverse(NULL)
    ),
    c(TRUE, TRUE, TRUE)
  )

  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = NULL,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(FALSE)
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(FALSE)
    ),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(FALSE)
    ),
    c(FALSE, FALSE, FALSE)
  )

  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$use_tidyverse(FALSE)
    ),
    c(FALSE, FALSE, FALSE)
  )
})

test_that("TransformationSequence add/insert/remove/head retains tidyverse setting", {
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$insert(Transformation$new(tidyverse = FALSE), 0)
    ),
    c(FALSE, FALSE, TRUE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$insert(Transformation$new(tidyverse = FALSE), 0)
    ),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$insert(Transformation$new(tidyverse = FALSE), 0)
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )

  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$add(Transformation$new(tidyverse = FALSE))
    ),
    c(FALSE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$add(Transformation$new(tidyverse = FALSE))
    ),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$add(Transformation$new(tidyverse = FALSE))
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )

  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$remove(1)
    ),
    c(TRUE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$remove(1)
    ),
    c(FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$remove(1)
    ),
    c(TRUE, TRUE)
  )

  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df",
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$head(2)
    ),
    c(FALSE, TRUE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = FALSE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$head(2)
    ),
    c(FALSE, FALSE)
  )
  expect_equal(
    get_tidyverse_status(
      TransformationSequence$new(
        name_in = "df", tidyverse = TRUE,
        list(Transformation$new(tidyverse = FALSE), Transformation$new(tidyverse = TRUE), Transformation$new())
      )$head(2)
    ),
    c(TRUE, TRUE)
  )
})
