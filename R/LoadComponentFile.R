LoadComponentFile <- R6::R6Class(
  "LoadComponentFile",
  inherit = LoadComponent,

  private = list(
    .file = NULL,
    .params = NULL,

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
      make.names(get_file_name_no_ext(private$.file))
    },

    get_code = function() {
      LoadComponentFile$code_file_type(private$.file, params = private$.params)
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
    initialize = function(file = NULL, params = NULL) {
      options(rlib_name_repair_verbosity = "quiet")

      # If the file is the result of a failed shiny req() call, treat it as NULL instead of error
      file <- tryCatch(file, error = function(err) NULL)

      private$.params <- params
      super$initialize()
      private$set_file(file)
      invisible(self)
    }
  )
)

LoadComponentFile$FILE_READ_EXTENSIONS <- c(
  ".csv", ".txt", ".sas7bdat", ".xpt", ".sav", ".zsav", ".dta", ".por", ".xls", ".xlsx"
)

LoadComponentFile$code_file_type <- function(url, params = list()) {
  ext <- tolower(paste0(".", tools::file_ext(url)))
  if (ext == ".") {
    stop("File must have an extension")
  }

  file_param <- shQuote(url, type = 'cmd')
  if (ext == ".csv" || ext == ".txt") {
    fxn <- "read.csv"
  } else if (ext == ".sas7bdat") {
    fxn <- "haven::read_sas"
    params[[".name_repair"]] <- "universal"
  } else if (ext == ".xpt") {
    fxn <- "haven::read_xpt"
    params[[".name_repair"]] <- "universal"
  } else if (ext == ".sav" || ext == ".zsav") {
    fxn <- "haven::read_sav"
    params[[".name_repair"]] <- "universal"
  } else if (ext == ".dta") {
    fxn <- "haven::read_dta"
    params[[".name_repair"]] <- "universal"
  } else if (ext == ".por") {
    fxn <- "haven::read_por"
    params[[".name_repair"]] <- "universal"
  } else if (ext == ".xls" || ext == ".xlsx") {
    fxn <- "readxl::read_excel"
    params[[".name_repair"]] <- "universal"
  } else {
    stop("Non supported file type: ", ext)
  }

  params_str <- lapply(names(params), function(param) {
    if (inherits(params[[param]], "character")) {
      glue::glue(", {param} = {shQuote(params[[param]], type = 'cmd')}")
    } else {
      glue::glue(", {param} = {params[[param]]}")
    }
  })
  params_str <- paste(params_str, collapse = "")
  glue::glue("{fxn}({file_param}{params_str})")
}

load_file_params_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "load-file-params-section flex flex-gap2",
      shinyjs::hidden(
        shinyWidgets::prettyCheckbox(
          ns("header"), "First row is header?", value = TRUE, width = "auto", shape = "curve", status = "primary", inline = TRUE
        ),
        shinyWidgets::pickerInput(
          ns("sep"), "Column separator", c(", (Comma)" = ",", ". (Period)" = ".", "Tab" = "\\t", "; (Semicolon)" = ";", "Any whitespace" = "")
        ),
        shinyWidgets::pickerInput(
          ns("dec"), "Decimal point", c(". (Period)" = ".", ", (Comma)" = ",")
        ),
        shinyWidgets::prettyCheckbox(
          ns("col_names"), "First row is header?", value = TRUE, width = "auto", shape = "curve", status = "primary", inline = TRUE
        ),
        numericInput(
          ns("sheet"), "Sheet #", 1, min = 1
        )
      )
    ),
    tags$hr()
  )
}

#' @param file_path (reactive string) Path to a file.
load_file_params_server <- function(id, file_path) {

  FILE_TYPE_PARAMS <- list(
    "csv" = c("header", "sep", "dec"),
    "txt" = c("header", "sep", "dec"),
    "xls" = c("col_names", "sheet"),
    "xlsx" = c("col_names", "sheet")
  )

  PARAM_DEFAULTS <- list(
    "header" = TRUE,
    "sep" = ",",
    "dec" = ".",
    "col_names" = TRUE,
    "sheet" = 1
  )

  moduleServer(
    id,
    function(input, output, session) {

      params_to_show <- reactive({
        if (is.null(file_path())) {
          return(NULL)
        }
        ext <- tools::file_ext(file_path())
        FILE_TYPE_PARAMS[[ext]]
      })

      observeEvent(params_to_show(), ignoreNULL = FALSE, {
        all_params <- unique(unlist(FILE_TYPE_PARAMS))
        sapply(all_params, shinyjs::hide)
        sapply(params_to_show(), shinyjs::show)
      })

      params <- reactive({
        if (is.null(params_to_show())) {
          return(NULL)
        }
        params <- sapply(params_to_show(), simplify = FALSE, function(param) {
          if (!is.na(input[[param]]) && input[[param]] == PARAM_DEFAULTS[[param]]) {
            NULL
          } else {
            input[[param]]
          }
        })
        params <- Filter(Negate(is.null), params)
        params
      })

      return(params)
    }
  )
}
