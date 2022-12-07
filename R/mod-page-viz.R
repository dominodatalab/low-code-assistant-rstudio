page_viz_ui <- function(id) {
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

    title_bar_ui(ns("title"), "Visualizations"),

    div(
      class = "page-main-content",
      shinyjs::hidden(
        div(
          id = ns("data_select"),
          br(),
          data_environment_ui(ns("data_select_mod"))
        )
      ),

      shinyjs::hidden(
        div(
          id = ns("main_section"),

          div(
            esquisse::esquisse_ui(
              ns("plot_module"),
              header = FALSE,
              controls = c("labs", "parameters", "appearance", "code")
            ),
            br()
          )
        )
      )
    ),

    div(
      class = "page-actions flex flex-gap2",
      textInput(ns("plot_name"), "Variable name", "plot"),
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

page_viz_server <- function(id, data_name_in = NULL) {

  init_data_name <- get_data_name_str(data_name_in)

  moduleServer(
    id,
    function(input, output, session) {

      shinymixpanel::mp_track(
        MIXPANEL_EVENT_INIT,
        list(
          section = MIXPANEL_SECTION_VIZ
        )
      )

      observeEvent(input$close, {
        kill_app()
      })

      #--- Dealing with the input dataset

      if (is.null(init_data_name)) {
        shinyjs::show("data_select")

        data_select_mod <- data_environment_server("data_select_mod")

        observeEvent(data_select_mod$name(), {
          shinyjs::hide("data_select")
          shinyjs::show("main_section")
        })
        data_name <- reactive({
          data_select_mod$name()
        })
      } else {
        data_name_in_r <- make_reactive(init_data_name)
        shinyjs::show("main_section")

        data_name <- reactive({
          req(data_name_in_r())
          data_name_in_r()
        })
      }

      #--- Dealing with the dataset

      main_data <- reactive({
        req(data_name())
        get(data_name(), envir = .GlobalEnv)
      })

      plot_module <- esquisse::esquisse_server(
        "plot_module",
        main_data,
        name = data_name,
        import_from = NULL,
        default_aes = c("fill", "color", "size", "shape")
      )

      code <- reactive({
        if (!nzchar(input$plot_name)) {
          return("")
        }
        if (is.null(plot_module$code_plot)) {
          return("")
        }
        paste0("library(ggplot2)\n", input$plot_name, " <- ", plot_module$code_plot)
      })

      #--- Insert code

      observe({
        shinyjs::toggleState("continue", condition = nzchar(code()))
      })

      observeEvent(input$continue, {
        insert_text(paste0(code()))

        shinymixpanel::mp_track(
          MIXPANEL_EVENT_CODE,
          list(
            section = MIXPANEL_SECTION_VIZ
          )
        )

        kill_app()
      })
    }
  )
}
