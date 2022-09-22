page_viz_ui <- function(id) {
  ns <- NS(id)

  tagList(
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
    )
  )
}

page_viz_server <- function(id, data_in, name_in) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(data_in(), {
        updateSelectInput(session, "var_x", choices = c("", names(data_in())))
        updateSelectInput(session, "var_y", choices = c("", names(data_in())))
        updateSelectInput(session, "color", choices = c("", names(data_in())))
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
        code <- glue::glue(code, "\n{input$plot_name} <- ggplot({name_in()})")
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
    }
  )
}

