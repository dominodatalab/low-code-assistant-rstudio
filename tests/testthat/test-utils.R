test_that("%||% works", {
  expect_equal("a" %||% "b", "a")
  expect_equal(c("a", "b") %||% "c", c("a", "b"))
  expect_equal(NULL %||% "a", "a")
  expect_equal("a" %||% NULL, "a")
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% NA, NA)
  expect_equal(NA %||% NULL, NA)
  expect_equal(NULL %||% c("a", "b"), c("a", "b"))
})

test_that("firstup works", {
  expect_equal(firstup("one"), "One")
  expect_equal(firstup(".one"), ".one")
  expect_equal(firstup("one two"), "One two")
  expect_equal(firstup("a"), "A")
})

test_that("isValidName works", {
  expect_true(isValidName("one"))
  expect_true(isValidName(".one"))
  expect_true(isValidName("one.two"))
  expect_true(isValidName("a5"))
  expect_true(isValidName("a_b"))

  expect_false(isValidName("5a"))
  expect_false(isValidName("_ab"))
  expect_false(isValidName("one!"))
  expect_false(isValidName(" one"))
  expect_false(isValidName("one two"))
  expect_false(isValidName("one-two"))
  expect_false(isValidName("one:two"))
})
