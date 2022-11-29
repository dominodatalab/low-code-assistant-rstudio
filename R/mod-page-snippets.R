page_snippets_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinymixpanel::mp_init(
      token = MIXPANEL_TOKEN,
      userid = get_user_id(),
      options = MIXPANEL_CONFIG,
      default_properties = list(
        Domino_version = get_domino_version(),
        LCA_version = as.character(utils::packageVersion(PACKAGE_NAME)),
        LCA_language = "R"
      ),
      default_properties_js = list("domain" = "location.host"),
      test_token = MIXPANEL_TEST_TOKEN,
      test_domains = MIXPANEL_TEST_DOMAINS,
      track_server = TRUE
    ),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(force = TRUE),
    html_dependency_lca(),
    waiter::useWaiter(),

    title_bar_ui(ns("title"), "Snippets"),

    div(
      class = "page-main-content",
      fluidRow(
        column(
          6,
          br(),
          shinyfilebrowser::path_browser_ui(ns("snippets"), bigger = TRUE)
        ),
        column(
          6,
          div(class = "snippets-file-name", textOutput(ns("file_name"))),
          shinycodeviewer::code_viewer_ui(ns("file_contents"))
        )
      )
    ),

    div(
      class = "page-actions flex flex-gap2",
      shinytip::tip(
        div(
          class = "action-button snippet-edit-mode-btn col-grey",
          id = ns("enable_edit"),
          span(
            class = "fa-stack",
            icon("pen", class = "fa-stack-1x"),
            icon("slash", class = "fa-stack-1x")
          )
        ),
        "Snippet editing disabled. Click to enable",
        position = "right"
      ),
      shinyjs::hidden(
        shinytip::tip(
          div(
            class = "action-button snippet-edit-mode-btn col-link",
            id = ns("disable_edit"),
            span(
              class = "fa-stack",
              icon("pen", class = "fa-stack-1x")
            )
          ),
          "Snippet editing enabled. Click to disable",
          position = "right"
        ),
        div(
          id = ns("editing_btns"),
          actionButton(
            ns("add"),
            "Add",
            icon = icon("plus"),
            class = "btn-primary btn-lg"
          ),
          shinyjs::hidden(
            actionButton(
              ns("edit"),
              "Edit",
              icon = icon("pencil"),
              class = "btn-primary btn-lg"
            )
          ),
          shinyjs::hidden(
            actionButton(
              ns("delete"),
              "Delete",
              icon = icon("trash"),
              class = "btn-primary btn-lg"
            )
          )
        )
      ),
      div(class = "flex-push"),
      actionButton(
        ns("close"),
        "Close",
        icon = icon("close"),
        class = "btn-lg"
      ),
      actionButton(
        ns("continue"),
        "Insert Code",
        icon = icon("check"),
        class = "btn-primary btn-lg"
      )
    )
  )
}

page_snippets_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      shinymixpanel::mp_track(
        MIXPANEL_EVENT_INIT,
        list(
          section = MIXPANEL_SECTION_SNIPPETS
        )
      )

      observeEvent(input$close, {
        kill_app()
      })

      snippets_refresh <- reactive_trigger()

      all_snippets <- reactive({
        snippets_refresh$depend()
        merge_all_snippets()
      })

      editable_paths_list <- reactive({
        waiter::waiter_show(color = "#333e48cc", html = waiter::spin_plus())
        on.exit(waiter::waiter_hide(), add = TRUE)
        get_editable_snippets_paths()
      })

      snippet_selector <- shinyfilebrowser::path_browser_server(
        "snippets",
        reactive(sort(all_snippets()$short_path)),
        show_extension = FALSE,
        text_empty = "No snippets here"
      )

      selected_snippet <- reactive({
        snippet_selector$selected()
      })

      selected_snippet_full <- reactive({
        all_snippets()[all_snippets()$short_path == selected_snippet(), ]$full_path
      })

      output$file_name <- renderText({
        if (is.null(selected_snippet())) {
          "Please select a snippet to preview"
        } else {
          get_file_name_no_ext(selected_snippet())
        }
      })

      file_contents <- reactive({
        if (is.null(selected_snippet())) {
          NULL
        } else {
          lines <- suppressWarnings(readLines(selected_snippet_full()))
          lines
        }
      })

      shinycodeviewer::code_viewer_server("file_contents", chunks = file_contents, editable = FALSE, show_chunk_numbers = TRUE)

      snippet_modal <- snippet_add_edit_modal("snippet_modal", editable_paths = editable_paths_list)

      observeEvent(snippet_modal$update(), {
        snippets_refresh$trigger()
      })

      observeEvent(input$add, {
        snippet_modal$show(action = "add", folder = snippet_selector$path())
      })

      observeEvent(input$edit, {
        snippet_modal$show(action = "edit", folder = snippet_selector$path(), snippet = selected_snippet_full())
      })

      observeEvent(input$delete, {
        shinyalert::shinyalert(
          title = "Are you sure you want to delete this snippet?",
          text = get_file_name_no_ext(selected_snippet()),
          showCancelButton = TRUE, cancelButtonText = "Cancel", confirmButtonText = "Delete",
          inputId = "delete_confirm", closeOnClickOutside = TRUE
        )
      })

      observeEvent(input$delete_confirm, {
        if (!input$delete_confirm) {
          return()
        }
        tryCatch({
          file.remove(selected_snippet_full())
          snippets_refresh$trigger()
          shinyalert::shinyalert(type = "success", "Snippet deleted")
        }, error = function(err) {
          shinyalert::shinyalert(type = "error", title = "Error", text = err$message)
        })
      })

      can_edit_file <- reactive({
        if (is.null(selected_snippet())) {
          return(FALSE)
        }
        editable_paths <- unname(unlist(editable_paths_list()))
        any(sapply(editable_paths, is_subdir, selected_snippet_full()))
      })

      observe({
        shinyjs::toggle("edit", condition = !is.null(selected_snippet()))
        shinyjs::toggle("delete", condition = !is.null(selected_snippet()))
      })

      observe({
        shinyjs::toggleState("edit", condition = can_edit_file())
        shinyjs::toggleState("delete", condition = can_edit_file())
      })

      observe({
        shinyjs::toggleState("continue", condition = !is.null(selected_snippet()))
      })

      observeEvent(input$enable_edit, {
        shinyjs::hide("enable_edit")
        shinyjs::show("disable_edit")
        shinyjs::show("editing_btns")
      })
      observeEvent(input$disable_edit, {
        shinyjs::show("enable_edit")
        shinyjs::hide("disable_edit")
        shinyjs::hide("editing_btns")
      })

      observeEvent(input$continue, {
        insert_text(paste(file_contents(), collapse = "\n"))

        shinymixpanel::mp_track(
          MIXPANEL_EVENT_CODE,
          list(
            section = MIXPANEL_SECTION_SNIPPETS
          )
        )

        kill_app()
      })
    }
  )
}
