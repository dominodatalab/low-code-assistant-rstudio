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
    existing_contents <- rstudioapi::getSourceEditorContext()$contents
    existing_library_calls <- existing_contents[grepl("^library(.+)$", existing_contents)]
    text <- remove_duplicate_lines(text, existing_library_calls)

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

remove_duplicate_lines <- function(text = "", lines_to_remove = c()) {
  for (line in lines_to_remove) {
    # Deal with lines in the middle
    text <- gsub(paste0("\n", line, "\n"), "\n", text, fixed = TRUE)

    # Deal with first line
    if (startsWith(text, paste0(line, "\n"))) {
      text <- sub(paste0(line, "\n"), "", text, fixed = TRUE)
    }

    # Deal with last line
    if (endsWith(text, paste0("\n", line))) {
      text <- paste0(text, "\n")
      text <- sub(paste0("\n", line, "\n"), "", text, fixed = TRUE)
    }

    if (text == line) {
      text <- ""
    }
  }
  text
}
