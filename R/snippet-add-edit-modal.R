snippet_add_edit_modal <- function(id, editable_paths) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      editable_paths_r <- make_reactive(editable_paths)

      action <- reactiveValEvent(NULL)
      folder <- reactiveValEvent(NULL)
      snippet <- reactiveValEvent(NULL)

      update <- reactiveVal(0)

      dialog <- modalDialog(
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          shinyjs::hidden(
            actionButton(ns("add"), "Add", class = "btn-primary", icon = icon("plus")),
            actionButton(ns("edit"), "Edit", class = "btn-primary", icon = icon("pencil"))
          )
        ),
        h2(textOutput(ns("title"), inline = TRUE), "snippet"),
        fluidRow(
          column(
            6,
            textInput(ns("name"), "Snippet Name", width = "100%")
          ),
          column(
            6,
            shinyWidgets::pickerInput(ns("repo"), "Project", "Current Project", width = "100%")
          )
        ),
        tags$strong("Folder:"),
        textOutput(ns("dir"), inline = TRUE),
        br(), br(),
        shinyAce::aceEditor(
          ns("contents"),
          value = "",
          mode = "r",
          height = "200px",
          fontSize = 14,
          debounce = 500
        )
      )

      show <- function(action = c("add", "edit"), folder, snippet = NULL) {
        action <- match.arg(action)

        if (action == "edit" && is.null(snippet)) {
          stop("Must provide a snippet for edit", call. = FALSE)
        }

        showModal(dialog)

        if (action == "add") {
          shinyWidgets::updatePickerInput(session, "repo", choices = editable_paths_r())
          shinyjs::show(ns("add"), asis = TRUE)
        } else if (action == "edit") {
          editable_paths <- unname(unlist(editable_paths_r()))
          path_idx <- which(sapply(editable_paths, is_subdir, snippet))
          repo <- editable_paths[path_idx]
          name <- get_file_name_no_ext(snippet)
          contents <- suppressWarnings(paste(readLines(snippet), collapse = "\n"))

          updateTextInput(session, "name", value = name)
          shinyWidgets::updatePickerInput(session, "repo", choices = editable_paths_r(), selected = repo)
          shinyAce::updateAceEditor(session, "contents", value = contents)
          shinyjs::disable(ns("name"), asis = TRUE)
          shinyjs::disable(ns("repo"), asis = TRUE)
          shinyjs::show(ns("edit"), asis = TRUE)
        }

        action(action)
        folder(folder)
        snippet(snippet)
      }

      output$title <- renderText({
        firstup(action())
      })

      output$dir <- renderText({
        folder()
      })

      observeEvent(input$add, {
        tryCatch({
          snippet_file_name <- paste0(input$name, ".R")
          snippet_dir <- file.path(input$repo, "snippets", folder())
          if (!dir.exists(snippet_dir)) {
            dir.create(snippet_dir, showWarnings = FALSE, recursive = TRUE)
          }
          snippet_path <- file.path(snippet_dir, snippet_file_name)
          writeLines(input$contents, snippet_path)
          removeModal()
          update(update() + 1)
          shinyalert::shinyalert(type = "success", "Snippet added", confirmButtonCol = "#337ab7")
        }, error = function(err) {
          shinyalert::shinyalert(type = "error", title = "Error", text = err$message, confirmButtonCol = "#337ab7")
        })
      })

      observeEvent(input$edit, {
        tryCatch({
          writeLines(input$contents, snippet())
          removeModal()
          update(update() + 1)
          shinyalert::shinyalert(type = "success", "Snippet edited", confirmButtonCol = "#337ab7")
        }, error = function(err) {
          shinyalert::shinyalert(type = "error", title = "Error", text = err$message, confirmButtonCol = "#337ab7")
        })
      })

      validated <- reactive({
        req(action())
        if (action() == "edit") {
          nzchar(trimws(input$contents))
        } else {
          nzchar(trimws(input$contents)) && nzchar(trimws(input$name)) &&
            !grepl("/", input$name, fixed = TRUE)
        }
      })

      observe({
        shinyjs::toggleState("add", condition = validated())
        shinyjs::toggleState("edit", condition = validated())
      })

      return(
        list(
          show = show,
          update = update
        )
      )
    }
  )
}
