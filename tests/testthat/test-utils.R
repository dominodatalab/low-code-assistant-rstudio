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

test_that("is_valid_name works", {
  expect_true(is_valid_name("one"))
  expect_true(is_valid_name(".one"))
  expect_true(is_valid_name("one.two"))
  expect_true(is_valid_name("a5"))
  expect_true(is_valid_name("a_b"))

  expect_false(is_valid_name("5a"))
  expect_false(is_valid_name("_ab"))
  expect_false(is_valid_name("one!"))
  expect_false(is_valid_name(" one"))
  expect_false(is_valid_name("one two"))
  expect_false(is_valid_name("one-two"))
  expect_false(is_valid_name("one:two"))
})
