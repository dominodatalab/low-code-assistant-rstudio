test_that("builtin snippets code don't error", {
  snippet_files <- list.files(
    system.file("sample_snippets", "snippets", package = PACKAGE_NAME),
    pattern = ".R", recursive = TRUE, full.names = TRUE
  )

  expect_equal(length(snippet_files), 27)

  skip_on_ci()
  skip_on_cran()

  init_search <- search()
  sapply(snippet_files, function(file) {
    # make sure that each snippet is run with a fresh set of packages loaded
    new_search <- setdiff(search(), init_search)
    sapply(new_search, function(name) detach(name, character.only = TRUE))

    expect_error(
      quietly(eval(parse(file = file))),
      NA,
      info = paste("Snippet:", file)
    )
  })
})
