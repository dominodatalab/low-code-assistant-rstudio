FILE_READ_EXTENSIONS <- c(".csv", ".txt")

`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}

drop_null <- function(x) {
  Filter(Negate(is.null), x)
}

cat0 <- function(...) {
  cat(..., sep = "")
}

firstup <- function(s) {
  substr(s, 1, 1) <- toupper(substr(s, 1, 1))
  s
}

is_valid_name <- function(s) {
  make.names(s) == s
}

insert_text <- function(text, newline = TRUE) {
  id <- rstudioapi::getSourceEditorContext()$id
  if (is.null(id)) {
    id <- rstudioapi::documentNew("")
  }

  if (newline) {
    text <- paste0(text, "\n")
  }

  tryCatch({
    rstudioapi::setCursorPosition(Inf, id = id)
    loc <- rstudioapi::getSourceEditorContext()$selection
    # if the end of the document is not a new line, add a new line
    if (loc[[1]]$range$end[[2]] != 1) {
      text <- paste0("\n", text)
    }
  }, finally = {
    rstudioapi::insertText(id = id, text = text)
  }, error = function(e) {})
  invisible(NULL)
}

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
