test_that("LoadComponentDemo works", {
  dataset <- LoadComponentDemo$new()
  expect_null(dataset$data)
  expect_null(dataset$code)
  expect_null(dataset$name)
  expect_null(dataset$error)

  dataset <- LoadComponentDemo$new("")
  expect_null(dataset$data)
  expect_null(dataset$code)
  expect_null(dataset$name)
  expect_null(dataset$error)

  dataset <- LoadComponentDemo$new(shiny::req(FALSE))
  expect_null(dataset$data)
  expect_null(dataset$code)
  expect_null(dataset$name)
  expect_null(dataset$error)

  dataset <- LoadComponentDemo$new("badname")
  expect_null(dataset$data)
  expect_null(dataset$code)
  expect_null(dataset$name)
  expect_match(dataset$error, "Unknown dataset")

  dataset <- LoadComponentDemo$new("iris")
  expect_equal(dataset$data, iris)
  expect_equal(dataset$code, "datasets::iris")
  expect_equal(dataset$name, "iris")
  expect_null(dataset$error)

  sapply(LoadComponentDemo$DATASETS, function(name) {
    dataset <- LoadComponentDemo$new(name)
    expect_true(nrow(dataset$data) > 0, info = name)
    expect_match(dataset$code, name, info = name)
    expect_equal(dataset$name, name, info = name)
    expect_null(dataset$error, info = name)
  })
})
