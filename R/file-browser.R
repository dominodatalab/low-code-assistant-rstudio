FILEBROWSER_TYPE_PARENT <- "parent"
FILEBROWSER_TYPE_DIR <- "dir"
FILEBROWSER_TYPE_FILE <- "file"
FILEBROWSER_TYPES <- c(FILEBROWSER_TYPE_PARENT, FILEBROWSER_TYPE_DIR, FILEBROWSER_TYPE_FILE)
FILEBROWSER_PARENT_PATH <- ".."

#' File browser
#'
#' Display a simple file browser of the server-side file system.
#' @import shiny
file_browser_ui <- function(id, height = NULL) {
  ns <- NS(id)

  style <- if (!is.null(height)) paste0("height: ", htmltools::validateCssUnit(height))

  div(
    singleton(tags$head(tags$style(
      ".shiny-file-browser { overflow: auto; border: 1px solid #ddd; padding: 0.5rem; }
      .shiny-file-browser .current-wd { font-family: monospace; padding: 0.5rem; }
      .shiny-file-browser .file-list { user-select: none; font-size: 1.1em; padding: 0 0.1rem; }
      .shiny-file-browser .file-row { display: flex; cursor: pointer; transition: background 0.3s; }
      .shiny-file-browser .file-row:hover { background: #f6f6f6; }
      .shiny-file-browser .file-row:active { background: #ccc; }
      .shiny-file-browser .file-icon { margin-right: 2rem; }
      .shiny-file-browser .file-type-dir .file-contents,
      .shiny-file-browser .file-type-parent .file-contents { font-weight: bold; }
      .shiny-file-browser .file-meta { font-style: italic; }
      "
    ))),
    class = "shiny-file-browser",
    style = style,
    div(class = "current-wd", textOutput(ns("current_wd"))),
    div(class = "file-list", uiOutput(ns("file_list")))
  )
}

#' @param id Unique ID for the module
#' @param path (reactive or static) The initial path the file browser should show.
#' @param extensions (reactive or static) List of file extensions that should be shown.
#' If `NULL`, all file types are shown.
#' @param path_as_root (boolean) If `TRUE`, the default `path` will act as the filesystem root,
#' which means the user won't be able to navigate to its parent directory. Note that symbolic
#' links can get around this parameter, for example if there is a symbolic link that points to
#' the parent of `path`.
#' @param include_hidden (boolean) If `TRUE`, show hidden files and folders.
#' @param include_empty (boolean) If `TRUE`, show empty files (files with size 0 bytes).
#' @param show_extension (boolean) If `TRUE`, show file extensions in the file names.
#' @param show_size (boolean) If `TRUE`, show file sizes along the file names.
#' @param parent_text The text to use to indicate the parent directory.
#' @return List with reactive elements:
#'   - selected: The full normalized path of the selected file (`NULL` before a file is selected)
#'   - path: The full normalized path that is currently displayed in the file browser
#' @import shiny
file_browser_server <- function(
    id,
    path = getwd(),
    extensions = NULL,
    path_as_root = FALSE,
    include_hidden = FALSE,
    include_empty = TRUE,
    show_extension = TRUE,
    show_size = TRUE,
    parent_text = FILEBROWSER_PARENT_PATH
) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      path_r <- make_reactive(path)
      extensions_r <- make_reactive(extensions)

      wd <- reactiveVal(NULL)
      selected <- reactiveVal(NULL)

      observeEvent(path_r(), {
        wd(make_path(path_r()))
        selected(NULL)
      })

      output$current_wd <- renderText({
        wd()
      })

      at_root <- reactive({
        req(wd())
        path_as_root && (wd() == make_path(path_r()))
      })

      output$file_list <- renderUI({
        files_dirs <- get_files_dirs(path = wd(), extensions = extensions_r(), hidden = include_hidden)

        dirs_rows <- lapply(files_dirs$dirs, function(dir) {
          create_file_row(FILEBROWSER_TYPE_DIR, dir, ns = ns)
        })
        files_rows <- lapply(files_dirs$files, function(file) {
          size <- suppressWarnings(file.info(file.path(wd(), file)))$size
          if (size == 0 && !include_empty) {
            return(NULL)
          }

          if (show_size) {
            size <- natural_size(size)
          } else {
            size <- NULL
          }

          if (show_extension) {
            file_text <- file
          } else {
            file_text <- tools::file_path_sans_ext(file)
          }

          create_file_row(FILEBROWSER_TYPE_FILE, file, file_text, size, ns = ns)
        })

        dirs_rows <- drop_null(dirs_rows)
        files_rows <- drop_null(files_rows)

        if (at_root()) {
          parent_row <- NULL
        } else {
          parent_row <- create_file_row(FILEBROWSER_TYPE_PARENT, FILEBROWSER_PARENT_PATH, parent_text, ns = ns)
        }

        tagList(
          parent_row,
          dirs_rows,
          files_rows,
          if (length(dirs_rows) == 0 && length(files_rows) == 0) div("No files here")
        )
      })

      observeEvent(input$file_clicked, {
        if (input$file_clicked == FILEBROWSER_PARENT_PATH) {
          if (!at_root()) {
            wd( make_path(dirname(wd())) )
          }
          return()
        }

        fullpath <- file.path(wd(), input$file_clicked)
        info <- file.info(fullpath)
        if (is.na(info$isdir)) {
          return()
        }
        if (info$isdir) {
          wd( make_path(file.path(wd(), input$file_clicked)) )
        } else {
          selected( make_path(file.path(wd(), input$file_clicked)) )
        }
      })

      return(list(
        path = wd,
        selected = selected
      ))
    }
  )
}

make_path <- function(path) {
  normalizePath(path, winslash = "/")
}

get_files_dirs <- function(path, extensions = NULL, hidden = FALSE) {
  all_files <- list.files(path = path, all.files = hidden, full.names = FALSE, recursive = FALSE, no.. = TRUE)
  all_dirs <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)
  files <- sort(setdiff(all_files, all_dirs))
  dirs <- sort(intersect(all_files, all_dirs))

  if (length(extensions) > 0) {
    regex <- gsub("\\.", "\\\\.", paste0(extensions, "$", collapse = "|"))
    files <- files[grepl(regex, files)]
  }

  list(files = files, dirs = dirs)
}

create_file_row <- function(type = FILEBROWSER_TYPES, path, text = path, meta = NULL, ns = shiny::NS(NULL)) {
  type <- match.arg(type)

  if (type == FILEBROWSER_TYPE_PARENT) {
    icon_type <- "arrow-left"
  } else if (type == FILEBROWSER_TYPE_DIR) {
    icon_type <- "folder"
  } else if (type == FILEBROWSER_TYPE_FILE) {
    icon_type <- "file-alt"
  }

  if (!is.null(meta)) {
    meta <- tagList(
      "-",
      span(meta, class = "file-meta")
    )
  }

  div(
    class = paste0("file-row file-type-", type),
    onclick = create_file_onclick(path, ns = ns),
    div(icon(icon_type, class = "fa-fw"), class = "file-icon"),
    div(
      class = "file-contents",
      span(text, class = "file-name"),
      meta
    )
  )
}

create_file_onclick <- function(new_path, ns = shiny::NS(NULL)) {
  glue::glue(
    "Shiny.setInputValue('{{ ns('file_clicked') }}', '{{ new_path }}', {priority: 'event'})",
    .open = "{{", .close = "}}"
  )
}
