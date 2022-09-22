data_environment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::alert("Select a data.frame from your R workspace", status = "info"),
    selectInput(ns("dataframes"), NULL, ""),
    shinyWidgets::prettyCheckbox(ns("custom_name"), "Custom variable name", FALSE, shape = "curve", status = "info"),
    conditionalPanel(
      "input.custom_name", ns = ns,
      textInput(ns("varname"), NULL, "", placeholder = "Variable name")
    )
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
        df_choices <- c("There are no data.frames" = "")
      } else {
        df_choices <- c("List of data.frames" = "",
                        setNames(dataframes, paste(dataframes, df_dims)))
      }
      updateSelectInput(session, "dataframes", choices = df_choices)

      name <- reactive({
        req(input$dataframes)

        if (input$custom_name) {
          make.names(input$varname)
        } else {
          input$dataframes
        }
      })

      code <- reactive({
        req(input$dataframes)
        if (name() == input$dataframes) {
          input$dataframes
        } else {
          glue::glue("{name()} <- {input$dataframes}")
        }
      })

      observeEvent(code(), {
        data <- eval(parse(text = code()))
        result$name <- name()
        result$code <- code()
        result$data <- data
      })

      return(reactive(list(
        name = result$name,
        data = result$data,
        code = result$code
      )))
    }
  )
}

get_dataframes <- function() {
  all_obj_names <- ls(envir = .GlobalEnv)
  dfs <- lapply(all_obj_names, function(x) {
    obj <- get(x, envir = .GlobalEnv)
    if (is(obj, "data.frame")) {
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
    glue::glue("({nrow(obj)} rows x {ncol(obj)} cols)")
  })
}
