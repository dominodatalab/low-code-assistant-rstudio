file_browser_ui <- function(id, height = NULL) {
  ns <- NS(id)

  style <- if (!is.null(height)) paste0("height: ", htmltools::validateCssUnit(height))

  div(
    singleton(tags$head(tags$style(
      ".shiny-file-browser { overflow: auto; }
      .shiny-file-browser .file-list { user-select: none; font-size: 1.1em; padding: 0 0.1rem; margin-top: 0.5rem; }
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

file_browser_server <- function(id, path = getwd(), extensions = NULL, allow_back = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      wd <- reactiveVal(path)
      selected <- reactiveVal(NULL)

      output$current_wd <- renderText({
        wd()
      })

      create_onclick <- function(new_path) {
        glue::glue(
          "Shiny.setInputValue('{{ ns('file_clicked') }}', '{{ new_path }}', {priority: 'event'})",
          .open = "{{", .close = "}}"
        )
      }

      create_file_row <- function(type = c("parent", "file", "dir"), path, meta = NULL) {
        type <- match.arg(type)

        if (type == "parent") {
          path <- ".."
          icon_type <- "arrow-left"
        } else if (type == "dir") {
          icon_type <- "folder"
        } else if (type == "file") {
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
          onclick = create_onclick(path),
          div(icon(icon_type, class = "fa-fw"), class = "file-icon"),
          div(
            class = "file-contents",
            span(path, class = "file-name"),
            meta
          )
        )
      }

      output$file_list <- renderUI({
        all_files <- list.files(path = wd(), all.files = FALSE, full.names = TRUE, recursive = FALSE)
        dirs <- Filter(function(f) file.info(f)$isdir, all_files)
        files <- Filter(function(f) !file.info(f)$isdir, all_files)
        if (!is.null(extensions)) {
          regex <- gsub("\\.", "\\\\.", paste0(extensions, "$", collapse = "|"))
          files <- files[grepl(regex, files)]
        }

        dirs <- sort(basename(dirs))
        files <- sort(basename(files))

        dirs_rows <- lapply(dirs, function(dir) {
          create_file_row("dir", dir)
        })
        files_rows <- lapply(files, function(file) {
          size <- file.info(file.path(wd(), file))$size
          create_file_row("file", file, paste(size, "bytes"))
        })

        if (wd() == path && !allow_back) {
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
          if (wd() != path || allow_back) {
            wd( dirname(wd()) )
          }
          return()
        }

        fullpath <- file.path(wd(), input$file_clicked)
        info <- file.info(fullpath)
        if (is.na(info$isdir)) {
          return()
        }
        if (info$isdir) {
          wd( file.path(wd(), input$file_clicked) )
        } else {
          selected( file.path(wd(), input$file_clicked) )
        }
      })

      return(list(
        path = wd,
        selected = selected
      ))
    }
  )
}
