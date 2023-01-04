test_that("get_user_project_dir works", {
  expect_identical(
    withr::with_envvar(
      c("DOMINO_WORKING_DIR" = "a/b/c"),
      get_user_project_dir()
    ),
    "a/b/c"
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_WORKING_DIR" = NA),
      get_user_project_dir()
    ),
    getwd()
  )
})

test_that("get_user_datasets_dir works", {
  expect_identical(
    withr::with_envvar(
      c("DOMINO_DATASETS_DIR" = "a/b/c"),
      get_user_datasets_dir()
    ),
    "a/b/c"
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_DATASETS_DIR" = NA),
      get_user_datasets_dir()
    ),
    getwd()
  )
})

test_that("get_user_git_dir works", {
  expect_identical(
    withr::with_envvar(
      c("DOMINO_IMPORTED_CODE_DIR" = "a/b/c"),
      get_user_git_dir()
    ),
    "a/b/c"
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_IMPORTED_CODE_DIR" = NA),
      get_user_git_dir()
    ),
    ""
  )
})

test_that("get_user_git_repos works", {
  mockery::stub(get_user_git_repos, "get_user_git_dir", "")
  expect_identical(get_user_git_repos(), character(0))

  mockery::stub(get_user_git_repos, "get_user_git_dir",
                system.file("tests_data", "git_repos", package = PACKAGE_NAME))
  expect_identical(basename(get_user_git_repos()), c("repo1", "repo2", "repo3"))
})

test_that("get_imported_projects_dirs", {
  # remove all existing project dir envvars
  projects <- grep("^DOMINO_.*_WORKING_DIR$", names(Sys.getenv()), value = TRUE)
  for(proj in projects) {
    withr::local_envvar(setNames(NA, proj))
  }

  expect_identical(get_imported_projects_dirs(), character(0))

  withr::local_envvar(c("DOMINO_PROJ1_WORKING_DIR" = "a/b/c"))
  expect_identical(get_imported_projects_dirs(), c("a/b/c"))

  withr::local_envvar(c("DOMINO_PROJ2_WORKING_DIR" = "a/b/d"))
  expect_identical(get_imported_projects_dirs(), c("a/b/c", "a/b/d"))
})

test_that("get_project_name works", {
  expect_identical(
    withr::with_envvar(
      c("DOMINO_PROJECT_NAME" = "my great project"),
      get_project_name()
    ),
    "my great project"
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_PROJECT_NAME" = NA),
      get_project_name()
    ),
    "sample_project"
  )
})

test_that("get_user_upload_dir works", {
  mockery::stub(get_user_upload_dir, "get_user_datasets_dir", "/path/to/datasets")
  mockery::stub(get_user_upload_dir, "get_project_name", "test_proj")

  expect_identical(
    withr::with_envvar(
      c("DOMINO_IS_GIT_BASED" = NA),
      get_user_upload_dir()
    ),
    file.path("/path/to/datasets", "local", "test_proj")
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_IS_GIT_BASED" = "true"),
      get_user_upload_dir()
    ),
    file.path("/path/to/datasets", "test_proj")
  )
})

test_that("get_api_key works", {
  expect_error(
    withr::with_envvar(
      c("DOMINO_USER_API_KEY" = NA),
      get_api_key()
    )
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_USER_API_KEY" = "a1b2c3d4"),
      get_api_key()
    ),
    "a1b2c3d4"
  )
})

test_that("get_api_base works", {
  expect_error(
    withr::with_envvar(
      c("DOMINO_API_HOST" = NA),
      get_api_base()
    )
  )

  expect_identical(
    withr::with_envvar(
      c("DOMINO_API_HOST" = "https://test.dominodatalab.com"),
      get_api_base()
    ),
    "https://test.dominodatalab.com"
  )
})

test_that("get_domino_version works", {
  httptest::with_mock_api({
    withr::local_envvar(c("DOMINO_USER_API_KEY" = "testkey",
                          "DOMINO_API_HOST" = "https://trial.dominodatalab.com"))

    expect_identical(get_domino_version(), "1.2.3")

    mockery::stub(get_domino_version, "call_api", function(...) stop("test"))
    expect_identical(get_domino_version(), "0")

    mockery::stub(get_domino_version, "call_api", call_api)
  })

  expect_identical(httptest::without_internet(get_domino_version()), "0")
})

test_that("get_user_id works", {
  httptest::with_mock_api({
    withr::local_envvar(c("DOMINO_USER_API_KEY" = "testkey",
                          "DOMINO_API_HOST" = "https://trial.dominodatalab.com"))

    expect_identical(get_user_id(), "123456abcdef")

    mockery::stub(get_user_id, "call_api", function(...) stop("test"))
    expect_identical(get_user_id(), "0")

    mockery::stub(get_user_id, "call_api", call_api)
  })

  expect_identical(httptest::without_internet(get_user_id()), "0")
})
