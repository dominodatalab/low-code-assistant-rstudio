LoadModuleFile <- R6::R6Class(
  "LoadModuleFile",
  inherit = LoadModule,

  private = list(
    .file = NULL,

    set_file = function(x) {
      if (!is.null(x) && x == "") {
        x <- NULL
      }
      private$.file <- x

      if (is.null(x)) {
        private$.name <- private$.code <- private$.data <- private$.error <- NULL
      } else {
        private$run()
      }
    },

    get_name = function() {
      make.names(tools::file_path_sans_ext(basename(private$.file)))
    },

    get_code = function() {
      LoadModuleFile$code_file_type(private$.file)
    },

    get_data = function() {
      suppressWarnings(eval(parse(text = private$.code)))
    }
  ),

  active = list(
    file = function(x) {
      if (missing(x)) private$.file
      else private$set_file(x)
    }
  ),

  public = list(
    initialize = function(file = NULL) {
      # If the file is the result of a failed shiny req() call, treat it as NULL instead of error
      file <- tryCatch(file, error = function(err) NULL)

      super$initialize()
      private$set_file(file)
      invisible(self)
    }
  )
)

LoadModuleFile$FILE_READ_EXTENSIONS <- c(
  ".csv", ".txt", ".tsv", ".sas7bdat", ".xpt", ".sav", ".zsav", ".dta", ".por", ".xls", ".xlsx"
)

LoadModuleFile$code_file_type <- function(url) {
  ext <- tolower(paste0(".", tools::file_ext(url)))
  if (ext == ".") {
    stop("File must have an extension")
  } else if (ext == ".csv" || ext == ".txt") {
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
