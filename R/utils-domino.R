get_user_project_dir <- function() {
  Sys.getenv("DOMINO_WORKING_DIR", getwd())
}

get_user_datasets_dir <- function() {
  Sys.getenv("DOMINO_DATASETS_DIR", getwd())
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
  resp <- call_api("/version")
  resp$version %||% ""
}

get_user_id <- function() {
  resp <- call_api("/v4/users/self")
  resp$id %||% ""
}
