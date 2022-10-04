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
    parent_text = ".."
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
        wd(normalizePath(path_r(), winslash = "/"))
        selected(NULL)
      })

      output$current_wd <- renderText({
        wd()
      })

      create_onclick <- function(new_path) {
        glue::glue(
          "Shiny.setInputValue('{{ ns('file_clicked') }}', '{{ new_path }}', {priority: 'event'})",
          .open = "{{", .close = "}}"
        )
      }

      create_file_row <- function(type = c("parent", "file", "dir"), name, size = NULL) {
        type <- match.arg(type)

        if (type == "parent") {
          name_text <- parent_text
          name_value <- ".."
          icon_type <- "arrow-left"
        } else if (type == "dir") {
          name_text <- name
          name_value <- name
          icon_type <- "folder"
        } else if (type == "file") {
          if (show_extension) {
            name_text <- name
          } else {
            name_text <- tools::file_path_sans_ext(name)
          }
          name_value <- name
          icon_type <- "file-alt"
        }

        if (!is.null(size)) {
          size <- tagList(
            "-",
            span(size, class = "file-meta")
          )
        }

        div(
          class = paste0("file-row file-type-", type),
          onclick = create_onclick(name_value),
          div(icon(icon_type, class = "fa-fw"), class = "file-icon"),
          div(
            class = "file-contents",
            span(name_text, class = "file-name"),
            size
          )
        )
      }

      output$file_list <- renderUI({
        wd()
        extensions_r()

        all_files <- list.files(path = wd(), all.files = include_hidden, full.names = TRUE, recursive = FALSE, no.. = TRUE)
        dirs <- Filter(function(f) file.info(f)$isdir, all_files)
        files <- Filter(function(f) !file.info(f)$isdir, all_files)
        if (!is.null(extensions_r())) {
          regex <- gsub("\\.", "\\\\.", paste0(extensions_r(), "$", collapse = "|"))
          files <- files[grepl(regex, files)]
        }

        dirs <- sort(basename(dirs))
        files <- sort(basename(files))

        dirs_rows <- lapply(dirs, function(dir) {
          create_file_row("dir", dir)
        })
        files_rows <- lapply(files, function(file) {
          size <- file.info(file.path(wd(), file))$size
          if (size == 0 && !include_empty) {
            return(NULL)
          }

          if (show_size) {
            size <- natural_size(size)
          } else {
            size <- NULL
          }
          create_file_row("file", file, size)
        })

        dirs_rows <- drop_null(dirs_rows)
        files_rows <- drop_null(files_rows)

        if (wd() == normalizePath(path_r(), winslash = "/") && path_as_root) {
          parent_row <- NULL
        } else {
          parent_row <- create_file_row("parent")
        }

        tagList(
          parent_row,
          dirs_rows,
          files_rows,
          if (length(dirs_rows) == 0 && length(files_rows) == 0) div("No files here")
        )
      })

      observeEvent(input$file_clicked, {
        if (input$file_clicked == "..") {
          if (wd() != normalizePath(path_r(), winslash = "/") || !path_as_root) {
            wd( normalizePath(dirname(wd()), winslash = "/") )
          }
          return()
        }

        fullpath <- file.path(wd(), input$file_clicked)
        info <- file.info(fullpath)
        if (is.na(info$isdir)) {
          return()
        }
        if (info$isdir) {
          wd( normalizePath(file.path(wd(), input$file_clicked), winslash = "/") )
        } else {
          selected( normalizePath(file.path(wd(), input$file_clicked), winslash = "/") )
        }
      })

      return(list(
        path = wd,
        selected = selected
      ))
    }
  )
}
