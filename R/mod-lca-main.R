lca_main_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
     page_data_select_ui(ns("data")),
    page_xforms_ui(ns("xforms"))
  )
}

lca_main_server <- function(id, data_name) {
  moduleServer(
    id,
    function(input, output, session) {

      data_mod <- page_data_select_server("data")
      data_name <- data_mod$data_name

      xforms_mod <- page_xforms_server("xforms", data_name)

      observeEvent(data_name(), {
        data_mod$hide()
        xforms_mod$show()
      })

      data_mod$show()
    }
  )
}
