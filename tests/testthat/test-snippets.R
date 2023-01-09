test_that("get_snippets_paths_builtin works", {
  expect_match(get_snippets_paths_builtin(), "sample_snippets/snippets")

  mockery::stub(get_snippets_paths_builtin, "system.file", "")
  expect_equal(get_snippets_paths_builtin(), character(0))
})

test_that("get_snippets_paths_git works", {
  mockery::stub(get_snippets_paths_git, "get_user_git_dir",
                system.file("tests_data", "git_repos", package = PACKAGE_NAME))
  paths <- get_snippets_paths_git()
  expect_length(paths, 2)
  expect_match(paths[1], "repo1/snippets")
  expect_match(paths[2], "repo3/snippets")
})

test_that("get_snippets_paths_imported_projects works", {
  mockery::stub(get_snippets_paths_imported_projects, "get_imported_projects_dirs",
                list.dirs(system.file("tests_data", "imported_projs", package = PACKAGE_NAME), recursive = FALSE))
  paths <- get_snippets_paths_imported_projects()
  expect_length(paths, 3)
  expect_match(paths[1], "proj1/snippets")
  expect_match(paths[2], "proj2/snippets")
  expect_match(paths[3], "proj3/snippets")
})

test_that("get_snippets_paths_current_project works", {
  mockery::stub(get_snippets_paths_current_project, "get_user_project_dir",
                system.file("tests_data", "curproj", package = PACKAGE_NAME))
  paths <- get_snippets_paths_current_project()
  expect_length(paths, 1)
  expect_match(paths[1], "curproj/snippets")
})

test_that("find_snippets_path works", {
  path <- tempdir()
  expect_equal(nrow(find_snippets_path(path)), 0)

  path <- system.file("tests_data", "git_repos", "repo1", package = PACKAGE_NAME)
  expect_equal(nrow(find_snippets_path(path)), 0)

  path <- system.file("tests_data", "git_repos", "repo1", "snippets", package = PACKAGE_NAME)
  expect_equal(nrow(find_snippets_path(path)), 3)

  path <- system.file("tests_data", "git_repos", "repo2", "snippets", package = PACKAGE_NAME)
  expect_equal(nrow(find_snippets_path(path)), 0)

  path <- system.file("tests_data", "git_repos", "repo3", "snippets", package = PACKAGE_NAME)
  expect_equal(nrow(find_snippets_path(path)), 2)
})

test_that("find_snippets works", {
  paths <- system.file("tests_data", "git_repos", "repo1", "snippets", package = PACKAGE_NAME)
  expect_equal(nrow(find_snippets(paths)), 3)

  paths <- c(
    system.file("tests_data", "git_repos", "repo1", "snippets", package = PACKAGE_NAME),
    system.file("tests_data", "git_repos", "repo2", "snippets", package = PACKAGE_NAME),
    system.file("tests_data", "git_repos", "repo3", "snippets", package = PACKAGE_NAME)
  )
  expect_equal(nrow(find_snippets(paths)), 4)
})

test_that("merge_all_snippets works", {
  mockery::stub(merge_all_snippets, "get_snippets_paths_current_project",
                system.file("tests_data", "curproj", "snippets", package = PACKAGE_NAME))
  mockery::stub(merge_all_snippets, "get_snippets_paths_git",
                system.file("tests_data", "git_repos", c("repo1", "repo2", "repo3"), "snippets", package = PACKAGE_NAME))
  mockery::stub(merge_all_snippets, "get_snippets_paths_imported_projects",
                system.file("tests_data", "imported_projs", c("proj1", "proj2", "proj3"), "snippets", package = PACKAGE_NAME))
  mockery::stub(merge_all_snippets, "get_snippets_paths_builtin",
                system.file("tests_data", "fake_builtin", "snippets", package = PACKAGE_NAME))

  expect_equal(nrow(merge_all_snippets()), 10)
})

test_that("get_editable_snippets_paths works", {
  mockery::stub(get_editable_snippets_paths, "get_writable_git_repos", character(0))
  mockery::stub(get_editable_snippets_paths, "get_user_project_dir",
                system.file("tests_data", "curproj", package = PACKAGE_NAME))
  expect_length(unlist(get_editable_snippets_paths()), 1)

  mockery::stub(get_editable_snippets_paths, "get_writable_git_repos",
                list.dirs(system.file("tests_data", "git_repos", package = PACKAGE_NAME), recursive = FALSE))
  expect_length(unlist(get_editable_snippets_paths()), 4)
})

test_that("add_snippet works", {
  withr::with_tempdir({
    path <- add_snippet("foo bar", name = "test file", repo = "new folder/second", local_folder = "")
    expect_equal(gsub("//", "/", path), file.path("new folder/second", "snippets", "test file.R"))
    expect_true(file.exists(path))
    expect_equal("foo bar", readLines(path))
  })

  withr::with_tempdir({
    path <- add_snippet("foo bar2", name = "test file2", repo = "another folder/second", local_folder = "a/b/c")
    expect_equal(path, file.path("another folder/second", "snippets", "a/b/c", "test file2.R"))
    expect_true(file.exists(path))
    expect_equal("foo bar2", readLines(path))
  })
})

test_that("edit_snippet works", {
  withr::with_file("snippet.R", {
    writeLines("some code", "snippet.R")
    edit_snippet("different code", "snippet.R")
    expect_equal("different code", readLines("snippet.R"))
  })
})
