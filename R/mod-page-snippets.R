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

      all_snippets <- merge_all_snippets()

      snippets <- shinyfilebrowser::path_browser_server(
        "snippets",
        all_snippets$short_path,
        show_extension = FALSE,
        text_empty = "No snippets here"
      )

      output$file_name <- renderText({
        if (is.null(snippets$selected())) {
          "Please select a snippet to preview"
        } else {
          tools::file_path_sans_ext(basename(snippets$selected()))
        }
      })

      file_contents <- reactive({
        if (is.null(snippets$selected())) {
          NULL
        } else {
          snippet_file <- all_snippets[all_snippets$short_path == snippets$selected(), ]$full_path
          lines <- suppressWarnings(readLines(snippet_file))
          lines
        }
      })

      shinycodeviewer::code_viewer_server("file_contents", chunks = file_contents, editable = FALSE)

      observe({
        shinyjs::toggleState("continue", condition = !is.null(snippets$selected()))
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
