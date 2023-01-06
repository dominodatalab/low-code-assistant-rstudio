test_that("builtin snippets code don't error", {
  snippet_files <- list.files(
    system.file("sample_snippets", "snippets", package = PACKAGE_NAME),
    pattern = ".R", recursive = TRUE, full.names = TRUE
  )

  expect_length(snippet_files, 27)

  skip_on_ci()
  skip_on_cran()

  # skip the tests if in an environment where one of the snippet packages isn't installed
  skip_if_not(nzchar(system.file(package = "Biostrings")))

  init_search <- search()
  sapply(snippet_files, function(file) {
    clean_search_path(init_search)

    expect_error(
      quietly(eval(parse(file = file))),
      NA,
      info = paste("Snippet:", file)
    )
  })
})
