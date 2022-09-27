lca_main_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    title_bar_ui(ns("title")),
    tabsetPanel(
      id = ns("main_tabs"),
      type = "hidden",
      tabPanelBody("data", page_data_select_ui(ns("data"), standalone = FALSE)),
      tabPanelBody("xforms", br(), page_xforms_ui(ns("xforms"), standalone = FALSE)),
      tabPanelBody("viz", page_viz_ui(ns("viz")))
    )
  )
}

lca_main_server <- function(id, data_name) {
  moduleServer(
    id,
    function(input, output, session) {

      title <- reactive({
        if (input$main_tabs == "data") {
          "Load Data"
        } else if (input$main_tabs == "xforms") {
          "Transformations"
        } else {
          "Visualizations"
        }
      })
      title_bar_server("title", title)

      data_mod <- page_data_select_server("data")
      xforms_mod <- page_xforms_server("xforms", data_mod$name)
      viz_mode <- page_viz_server("viz", xforms_mod$data, xforms_mod$name)

      observeEvent(data_mod$done(), {
        updateTabsetPanel(session, "main_tabs", "xforms")
      })

      observeEvent(xforms_mod$done(), {
        updateTabsetPanel(session, "main_tabs", "viz")
      })

    }
  )
}
