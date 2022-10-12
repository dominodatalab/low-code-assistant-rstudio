page_viz_ui <- function(id, standalone = TRUE) {
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
      test_domains = MIXPANEL_TEST_DOMAINS
    ),
    shinyjs::useShinyjs(),
    html_dependency_lca(),
    if (standalone) title_bar_ui(ns("title"), "Visualizations"),
    shinyjs::hidden(checkboxInput(ns("standalone"), NULL, standalone)),

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
        textInput(ns("plot_name"), "Variable name", "plot"),
        selectInput(ns("plot_type"), "Plot type", c("", "Scatter", "Histogram", "Line")),
        conditionalPanel(
          "input.plot_type != ''", ns = ns,
          fluidRow(
            column(4,selectInput(ns("var_x"), "X Variable", choices = "")),
            column(4,     conditionalPanel(
              "input.plot_type != 'Histogram'", ns = ns,
              selectInput(ns("var_y"), "Y Variable", choices = ""),
            )),
            column(4,  selectInput(ns("color"), "Color", choices = ""))
          ),
          fluidRow(
            column(4,shinyWidgets::prettySwitch(ns("log_x"), "Log X-axis", status = "primary", fill = TRUE),
                   shinyWidgets::prettySwitch(ns("plotly"), "Use plotly", status = "primary", fill = TRUE)),
            column(4, sliderInput(ns("font"), "Font size", 6, 50, 12)),
            column(4, selectInput(ns("theme"), "Theme", c("", "classic", "minimal", "grey", "bw", "linedraw", "light", "dark")))
          ),
          uiOutput(ns("code")),
          conditionalPanel(
            "input.plotly", ns = ns,
            plotly::plotlyOutput(ns("plot_plotly"), height = 600)
          ),
          conditionalPanel(
            "!input.plotly", ns = ns,
            plotOutput(ns("plot"), height = 600),
          )
        ),

        br(),
        div(
          class = "no-margin flex flex-gap2",
          actionButton(ns("close"), "Close"),
          div(class = "flex-push"),
          hide_if_standalone(
            standalone,
            shinyWidgets::prettyCheckbox(
              ns("insert_code"),
              "Insert Code",
              value = TRUE,
              width = "auto",
              shape = "curve",
              status = "primary"
            )
          ),
          actionButton(
            ns("continue"),
            if (standalone) "Apply" else "Finish",
            icon = icon("check"),
            class = "btn-primary btn-lg"
          )
        )
      )
    )
  )
}

page_viz_server <- function(id, data_in = NULL, name_in = NULL) {
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

      if (is.null(data_in)) {
        shinyjs::show("data_select")

        data_select_mod <- data_environment_server("data_select_mod")

        observeEvent(data_select_mod$name(), {
          shinyjs::hide("data_select")
          shinyjs::show("main_section")
        })
        data <- reactive({
          req(data_select_mod$data())
          data_select_mod$data()
        })
        name <- reactive({
          req(data_select_mod$name())
          data_select_mod$name()
        })
      } else {
        shinyjs::show("main_section")

        data <- reactive({
          req(data_in())
          data_in()
        })
        name <- reactive({
          req(name_in())
          name_in()
        })
      }

      observeEvent(data(), {
        updateSelectInput(session, "var_x", choices = c("", names(data())))
        updateSelectInput(session, "var_y", choices = c("", names(data())))
        updateSelectInput(session, "color", choices = c("", names(data())))
      })

      code <- reactive({
        req(input$var_x, nzchar(input$var_x))
        if (input$plot_type != "Histogram") {
          req(input$var_y, nzchar(input$var_y))
        }

        yvar <- if (input$plot_type != "Histogram") glue::glue(", y = {input$var_y}") else ""
        col <- if (nzchar(input$color)) glue::glue(", color = {input$color}, fill = {input$color}") else ""
        code <- "library(ggplot2)\n"
        if (input$plotly) {
          code <- paste0(code, "library(plotly)\n")
        }
        code <- glue::glue(code, "\n{input$plot_name} <- ggplot({name()})")
        code <- paste0(code, " + \n  aes")
        code <- glue::glue(code, "(x = {input$var_x}{yvar}{col})")

        if (input$plot_type == "Scatter") {
          code <- glue::glue(code, " + \n  geom_point()")
        } else if (input$plot_type == "Histogram") {
          code <- glue::glue(code, " + \n  geom_histogram()")
        } else if (input$plot_type == "Line") {
          code <- glue::glue(code, " + \n  geom_line()")
        }

        if (input$log_x) {
          code <- paste0(code, " + \n  scale_x_log10()")
        }

        if (input$theme == "") {
          code <- glue::glue(code, " + \n  theme(text = element_text(size = {input$font}))")
        } else {
          code <- glue::glue(code, " + \n  theme_{input$theme}(base_size = {input$font})")
        }

        if (input$plotly) {
          code <- glue::glue(code, "\n{input$plot_name} <- ggplotly({input$plot_name})\n")
        }
        code <- paste0(code, "\n", input$plot_name, "\n")

        code
      })

      plot <- reactive({
        eval(parse(text = code()))
      })

      output$plot <- renderPlot({
        plot()
      })

      output$plot_plotly <- plotly::renderPlotly({
        plot()
      })

      output$code <- renderUI({
        req(code())
        tags$pre(code())
      })


      observeEvent(input$continue, {
        if (input$insert_code) {
          insert_text(paste0(code()))

          shinymixpanel::mp_track(
            MIXPANEL_EVENT_CODE,
            list(
              section = MIXPANEL_SECTION_VIZ
            )
          )
        }

        kill_app()
      })
    }
  )
}
