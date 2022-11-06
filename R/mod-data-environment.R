data_environment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert("Select a data.frame from your R workspace", status = "info"),
    shinyfilebrowser::path_browser_ui(ns("dataframes"), bigger = TRUE)
  )
}

data_environment_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- reactiveValues(name = NULL, code = NULL, data = NULL)

      dataframes <- get_dataframes()
      df_dims <- get_df_dimensions(dataframes)
      if (length(df_dims) == 0) {
        df_choices <- c()
      } else {
        df_choices <- stats::setNames(dataframes, paste(dataframes, df_dims))
      }
      datasets <- shinyfilebrowser::path_browser_server(
        "dataframes",
        df_choices,
        show_path = FALSE,
        show_icons = FALSE,
        text_empty = "There are no data.frames"
      )

      name <- reactive({
        req(datasets$selected())
        datasets$selected()
      })

      code <- reactive({
        req(datasets$selected())
        datasets$selected()
      })

      observeEvent(code(), {
        data <- eval(parse(text = code()))
        result$name <- name()
        result$code <- code()
        result$data <- data
      })

      return(list(
        name = reactive(result$name),
        data = reactive(result$data),
        code = reactive(result$code)
      ))
    }
  )
}

get_dataframes <- function() {
  all_obj_names <- ls(envir = .GlobalEnv)
  dfs <- lapply(all_obj_names, function(x) {
    obj <- get(x, envir = .GlobalEnv)
    if (methods::is(obj, "data.frame")) {
      x
    } else {
      NULL
    }
  })
  dfs <- unlist(dfs)
  dfs
}

get_df_dimensions <- function(dfs) {
  lapply(dfs, function(df) {
    obj <- get(df, envir = .GlobalEnv)
    glue::glue("({ natural_num(nrow(obj)) } rows x { natural_num(ncol(obj)) } cols)")
  })
}
