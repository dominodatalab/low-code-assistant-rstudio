FILE_READ_EXTENSIONS <- c(".csv", ".txt", ".tsv", ".sas7bdat", ".xpt", ".sav", ".zsav", ".dta",
                          ".por", ".xls", ".xlsx")

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

get_load_code <- function(url) {
  ext <- tolower(paste0(".", tools::file_ext(url)))
  if (ext == ".csv" || ext == ".txt") {
    glue::glue("read.csv({shQuote(url, type = 'cmd')})")
  } else if (ext == ".tsv") {
    glue::glue("read.csv({shQuote(url, type = 'cmd')}, sep = '\t')")
  } else if (ext == ".sas7bdat") {
    glue::glue("haven::read_sas({shQuote(url, type = 'cmd')})")
  } else if (ext == ".xpt") {
    glue::glue("haven::read_xpt({shQuote(url, type = 'cmd')})")
  } else if (ext == ".sav" || ext == ".zsav") {
    glue::glue("haven::read_sav({shQuote(url, type = 'cmd')})")
  } else if (ext == ".dta") {
    glue::glue("haven::read_dta({shQuote(url, type = 'cmd')})")
  } else if (ext == ".por") {
    glue::glue("haven::read_por({shQuote(url, type = 'cmd')})")
  } else if (ext == ".xls" || ext == ".xlsx") {
    glue::glue("readxl::read_excel({shQuote(url, type = 'cmd')})")
  } else {
    stop("Non supported file type: ", ext)
  }
}
