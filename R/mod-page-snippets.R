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
    html_dependency_lca(),

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

      snippet_edit_mode_ui(ns("snippet-edit-buttons")),

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

      snippet_selector <- shinyfilebrowser::path_browser_server(
        "snippets",
        reactive(sort(all_snippets()$short_path)),
        clear_selection_on_navigate = TRUE,
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

      snippet_edit_mode_server(
        "snippet-edit-buttons",
        selected_snippet = selected_snippet,
        selected_snippet_full = selected_snippet_full,
        curwd = reactive(snippet_selector$path()),
        snippets_refresh = snippets_refresh
      )

      observe({
        shinyjs::toggleState("continue", condition = !is.null(selected_snippet()))
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
