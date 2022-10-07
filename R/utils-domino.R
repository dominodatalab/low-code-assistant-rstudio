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
