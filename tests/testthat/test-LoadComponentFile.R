test_that("LoadComponentFile basic works", {
  file <- LoadComponentFile$new()
  expect_null(file$data)
  expect_null(file$code)
  expect_null(file$error)
  expect_null(file$file)

  file <- LoadComponentFile$new("")
  expect_null(file$data)
  expect_null(file$code)
  expect_null(file$error)
  expect_null(file$file)

  file <- LoadComponentFile$new(shiny::req(FALSE))
  expect_null(file$data)
  expect_null(file$code)
  expect_null(file$error)
  expect_null(file$file)

  file$file <- "somefile"
  expect_equal(file$file, "somefile")
  expect_false(is.null(file$error))

  file$file <- "somefile"

  file$file <- system.file("tests_data", "load_data", "cars.csv", package = PACKAGE_NAME)
  expect_identical(file, LoadComponentFile$new(system.file("tests_data", "load_data", "cars.csv", package = PACKAGE_NAME)))
})

test_that("LoadComponentFile works", {
  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.csv", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read.csv")
  expect_equal(file$data, cars)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars bad $name.csv", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read.csv")
  expect_equal(file$data, cars)
  expect_equal(file$name, "cars.bad..name")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars", package = PACKAGE_NAME)
  )
  expect_null(file$code)
  expect_null(file$data)
  expect_null(file$name)
  expect_match(file$error, "extension")

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars_semicolon.txt", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read.csv")
  expect_equal(dim(file$data), c(50, 1))
  expect_equal(file$name, "cars_semicolon")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.blahblah", package = PACKAGE_NAME)
  )
  expect_null(file$code)
  expect_null(file$data)
  expect_null(file$name)
  expect_match(file$error, "non supported file type", ignore.case = TRUE)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars_wrong_format.sas7bdat", package = PACKAGE_NAME)
  )
  expect_null(file$code)
  expect_null(file$data)
  expect_null(file$name)
  expect_match(file$error, "failed to parse", ignore.case = TRUE)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.sas7bdat", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_sas")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.xpt", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_xpt")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.sav", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_sav")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.zsav", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_sav")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.dta", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_dta")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "world-95.por", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_por")
  expect_equal(dim(file$data), c(109, 26))
  expect_equal(file$name, "world.95")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.xlsx", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_excel")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.xls", package = PACKAGE_NAME)
  )
  expect_match(file$code, "read_excel")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)
})

test_that("LoadComponentFile with parameters works", {
  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars_semicolon.txt", package = PACKAGE_NAME),
    params = list(sep = ";")
  )
  expect_match(file$code, "read.csv")
  expect_equal(file$data, cars)
  expect_equal(file$name, "cars_semicolon")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.xlsx", package = PACKAGE_NAME),
    params = list(sheet = 2)
  )
  expect_equal(nrow(file$data), nrow(cars) - 1)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.xlsx", package = PACKAGE_NAME),
    params = list(sheet = 2, col_names = FALSE)
  )
  expect_match(file$code, "read_excel")
  expect_equal(file$data, cars, ignore_attr = TRUE)
  expect_equal(file$name, "cars")
  expect_null(file$error)

  file <- LoadComponentFile$new(
    system.file("tests_data", "load_data", "cars.xlsx", package = PACKAGE_NAME),
    params = list(sheet = 3)
  )
  expect_null(file$code)
  expect_null(file$data)
  expect_null(file$name)
  expect_match(file$error, "position 3.*2 sheet")
})
