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

test_that("cat0 works", {
  expect_output(cat0("one", "two"), "onetwo")
  expect_output(cat0("one ", " two"), "one  two")
})

test_that("firstup works", {
  expect_equal(firstup("one"), "One")
  expect_equal(firstup(".one"), ".one")
  expect_equal(firstup("one two"), "One two")
  expect_equal(firstup("a"), "A")
})

test_that("assign_to_global works", {
  rnd_name <- paste0("lca-test-assign-to-global-", sample(10000, 1))
  rnd_value <- sample(10000, 1)
  assign_to_global(rnd_name, rnd_value)
  on.exit(suppressWarnings(rm(list = c(rnd_name), envir = globalenv())), add = TRUE)
  expect_identical(get(rnd_name, envir = globalenv()), rnd_value)
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

test_that("remove_duplicate_lines works", {
  expect_equal(remove_duplicate_lines("a", "a"), "")

  text <- "a\nb\nc\nd\n b\nab\nb\nc\na\na"
  expect_equal(
    remove_duplicate_lines(text),
    text
  )
  expect_equal(
    remove_duplicate_lines(text, "a"),
    "b\nc\nd\n b\nab\nb\nc"
  )
  expect_equal(
    remove_duplicate_lines(text, "b"),
    "a\nc\nd\n b\nab\nc\na\na"
  )
  expect_equal(
    remove_duplicate_lines(text, "c"),
    "a\nb\nd\n b\nab\nb\na\na"
  )
  expect_equal(
    remove_duplicate_lines(text, " b"),
    "a\nb\nc\nd\nab\nb\nc\na\na"
  )
  expect_equal(
    remove_duplicate_lines(text, c("a", "b")),
    "c\nd\n b\nab\nc"
  )
  expect_equal(
    remove_duplicate_lines(text, c("a", "b", "ab")),
    "c\nd\n b\nc"
  )
  expect_equal(
    remove_duplicate_lines(text, c("a", "b", "c")),
    "d\n b\nab"
  )
  expect_equal(
    remove_duplicate_lines(text, c("a", "b", "c", "d")),
    " b\nab"
  )
  expect_equal(
    remove_duplicate_lines(text, c("a", "b", "c", "d", "ab")),
    " b"
  )
  expect_equal(
    remove_duplicate_lines(text, c("a", "b", "c", "d", "ab", " b")),
    ""
  )

  text <- "library(shiny)\nfirst\n#library(shiny)\nsecond\nlibraray(shiny)\nthird\nlibrary(shiny)\nlibrary(shinyalert)\nfourth\nlibrary(shiny)\nfifth\nlibrary(shinyjs)\nsixth\nlibrary(shinyalert)\n"
  expect_identical(
    remove_duplicate_lines(text),
    text
  )
  expect_identical(
    remove_duplicate_lines(text, "library(shiny)"),
    "first\n#library(shiny)\nsecond\nlibraray(shiny)\nthird\nlibrary(shinyalert)\nfourth\nfifth\nlibrary(shinyjs)\nsixth\nlibrary(shinyalert)\n"
  )
  expect_identical(
    remove_duplicate_lines(text, "library(shinyalert)"),
    "library(shiny)\nfirst\n#library(shiny)\nsecond\nlibraray(shiny)\nthird\nlibrary(shiny)\nfourth\nlibrary(shiny)\nfifth\nlibrary(shinyjs)\nsixth\n"
  )
  expect_identical(
    remove_duplicate_lines(text, "library(shinyjs)"),
    "library(shiny)\nfirst\n#library(shiny)\nsecond\nlibraray(shiny)\nthird\nlibrary(shiny)\nlibrary(shinyalert)\nfourth\nlibrary(shiny)\nfifth\nsixth\nlibrary(shinyalert)\n"
  )
  expect_identical(
    remove_duplicate_lines(text, c("library(shiny)", "library(shinyalert)")),
    "first\n#library(shiny)\nsecond\nlibraray(shiny)\nthird\nfourth\nfifth\nlibrary(shinyjs)\nsixth\n"
  )
})

test_that("get_writable_git_repos works", {
  mockery::stub(get_writable_git_repos, "get_user_git_repos", c())
  expect_identical(get_writable_git_repos(), character(0))
  mockery::stub(get_writable_git_repos, "get_user_git_repos", get_user_git_repos)

  mockery::stub(get_writable_git_repos, "get_user_git_dir", depth = 2,
                system.file("tests_data", "git_repos", package = PACKAGE_NAME))

  mockery::stub(get_writable_git_repos, "is_git_writable", FALSE)
  expect_identical(get_writable_git_repos(), character(0))

  mockery::stub(get_writable_git_repos, "is_git_writable",
                function(dir) {
                  basename(dir) %in% c("repo1", "repo3")
                })
  expect_identical(length(get_writable_git_repos()), 2L)
  expect_true(all(basename(get_writable_git_repos()) %in% c("repo1", "repo3")))
})

test_that("make_path works", {
  expect_identical(make_path(""), "")
  expect_identical(make_path("a"), file.path(getwd(), "a"))
  expect_identical(make_path("a/b/c"), file.path(getwd(), "a/b/c"))
  expect_identical(make_path("a/b/"), file.path(getwd(), "a/b//"))
})

test_that("is_subdir works", {
  expect_true(is_subdir("a", "a/b/c"))
  expect_true(is_subdir("a/b", "a/b/c"))
  expect_true(is_subdir("a/b/c", "a/b/c"))
  expect_false(is_subdir("a/b/c/d", "a/b/c"))
  expect_false(is_subdir("a/b/c/d", "a/b/c"))
  expect_true(is_subdir("", ""))
  expect_true(is_subdir("", "a"))
  expect_true(is_subdir("", "a/b"))
  expect_false(is_subdir("a", ""))
  expect_false(is_subdir("a/b", ""))
})

test_that("get_file_name_no_ext works", {
  expect_equal(get_file_name_no_ext("file"), "file")
  expect_equal(get_file_name_no_ext("path/file"), "file")
  expect_equal(get_file_name_no_ext("path/file.txt.gz"), "file.txt")
})

test_that("get_data_name_str works", {
  mock_env <- new.env(parent = emptyenv())
  mock_env$df1 <- mtcars
  mock_env$df2 <- iris
  mock_env$df3 <- "not a data frame"

  mockery::stub(get_data_name_str, 'globalenv', mock_env)

  expect_null(get_data_name_str())
  expect_null(get_data_name_str(""))
  expect_null(get_data_name_str(mock_env$df1))
  expect_null(get_data_name_str("test"))

  reactive_test <- shiny::reactive("test")
  expect_identical(get_data_name_str(reactive_test), reactive_test)
  expect_identical(get_data_name_str("df1"), "df1")
  expect_identical(get_data_name_str("df2"), "df2")
  expect_null(get_data_name_str("df3"))
})

test_that("quietly works", {
  expect_output(print("test"))
  expect_output(quietly(print("test")), NA)

  expect_message(message("test"))
  expect_message(quietly(message("test")), NA)

  expect_warning(warning("test"))
  expect_warning(quietly(print("test")), NA)
})
