get_user_project_dir <- function() {
  Sys.getenv("DOMINO_WORKING_DIR", getwd())
}

get_user_datasets_dir <- function() {
  Sys.getenv("DOMINO_DATASETS_DIR", getwd())
}

get_user_git_dir <- function() {
  Sys.getenv("DOMINO_IMPORTED_CODE_DIR", "")
}

get_user_git_repos <- function() {
  dir <- get_user_git_dir()
  if (dir == "") {
    character(0)
  } else {
    list.dirs(dir, recursive = FALSE)
  }
}

get_imported_projects_dirs <- function() {
  all_envvars <- Sys.getenv()
  projects_idx <- which(grepl("^DOMINO_.*_WORKING_DIR$", names(all_envvars)))
  if (length(projects_idx) == 0) {
    character(0)
  } else {
    as.character(all_envvars[projects_idx])
  }
}

get_user_upload_dir <- function() {
  git <- (Sys.getenv("DOMINO_IS_GIT_BASED") == "true")
  if (git) {
    file.path(
      Sys.getenv("DOMINO_DATASETS_DIR", getwd()),
      Sys.getenv("DOMINO_PROJECT_NAME", "sample_project")
    )
  } else {
    file.path(
      Sys.getenv("DOMINO_DATASETS_DIR", getwd()),
      "local",
      Sys.getenv("DOMINO_PROJECT_NAME", "sample_project")
    )
  }
}

get_api_key <- function() {
  key <- Sys.getenv("DOMINO_USER_API_KEY", "")
  if (key == "") {
    stop("Could not find envvar 'DOMINO_USER_API_KEY'", call. = FALSE)
  }
  key
}

get_api_base <- function() {
  url <- Sys.getenv("DOMINO_API_HOST", "")
  if (url == "") {
    stop("Could not find envvar 'DOMINO_API_HOST'", call. = FALSE)
  }
  url
}

get_domino_version <- function() {
  version <- tryCatch({
    resp <- call_api("/version")
    resp$version
  }, error = function(err) {
    "0"
  })
  version
}

get_user_id <- function() {
  id <- tryCatch({
    resp <- call_api("/v4/users/self")
    resp$id
  }, error = function(err) {
    "0"
  })
  id
}
