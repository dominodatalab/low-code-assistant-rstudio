`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}

cat0 <- function(...) {
  cat(..., sep = "")
}

firstup <- function(s) {
  substr(s, 1, 1) <- toupper(substr(s, 1, 1))
  s
}

assign_to_global <- function(name, x, pos = 1L) {
  assign(name, x, envir = as.environment(pos))
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
    if (endsWith(text, "\n\n")) {}
    else if (endsWith(text, "\n")) {
      text <- paste0(text, "\n")
    } else {
      text <- paste0(text, "\n\n")
    }
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

get_file_name_no_ext <- function(x) {
  tools::file_path_sans_ext(basename(x))
}
