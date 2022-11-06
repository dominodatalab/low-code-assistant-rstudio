get_snippets_paths_builtin <- function() {
  path <- system.file("sample_snippets", "snippets", package = "assistDomino")
  if (dir.exists(path)) {
    path
  } else {
    character(0)
  }
}

get_snippets_paths_git <- function() {
  Sys.glob(file.path(Sys.getenv("DOMINO_IMPORTED_CODE_DIR"),"*", "snippets"))
}

get_snippets_paths_imported_projects <- function() {
  file.path(as.character(Sys.getenv()[grepl("^DOMINO_.*_WORKING_DIR$", names(Sys.getenv()))]), "snippets")
}

get_snippets_paths_current_project <- function() {
  file.path(Sys.getenv("DOMINO_WORKING_DIR"), "snippets")
}

find_snippets <- function(paths) {
  dfs <- lapply(paths, find_snippets_path)
  df <- Reduce(function(...) merge(..., all = TRUE), dfs)
  df <- df[!duplicated(df$short_path), ]
  df
}

find_snippets_path <- function(path) {
  empty_df <- data.frame(full_path = character(0), short_path = character(0))
  if (!dir.exists(path)) {
    return(empty_df)
  }
  if (basename(path) != "snippets") {
    return(empty_df)
  }
  data.frame(
    full_path = list.files(path, recursive = TRUE, pattern = "\\.py$", full.names = TRUE),
    short_path = list.files(path, recursive = TRUE, pattern = "\\.py$", full.names = FALSE),
    stringsAsFactors = FALSE
  )
}

merge_snippets <- function() {
  paths <- c(
    get_snippets_paths_current_project(),
    get_snippets_paths_git(),
    get_snippets_paths_imported_projects(),
    get_snippets_paths_builtin()
  )
  find_snippets(paths)
}

runsnippetsapp <- function() {
  library(shiny)
  library(shinyfilebrowser)

  ui <- fluidPage(
    fluidRow(
      column(
        6,
        path_browser_ui("snippets", bigger = TRUE)
      ),
      column(
        6,
        uiOutput("preview")
      )
    )
  )

  server <- function(input, output, session) {
    all_snippets <- merge_snippets()

    snippets <- path_browser_server("snippets", all_snippets$short_path, show_extension = FALSE)

    output$preview <- renderUI({
      if (is.null(snippets$selected())) {
        return(h2("Please select a snippet to preview"))
      }

      tagList(
        h2(tools::file_path_sans_ext(basename(snippets$selected()))),
        tags$pre(paste(suppressWarnings(readLines(all_snippets[all_snippets$short_path == snippets$selected(), ]$full_path)), collapse = "\n"))
      )
    })
  }

  shinyApp(ui, server)

}
