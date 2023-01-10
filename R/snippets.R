get_snippets_paths_builtin <- function() {
  path <- system.file("sample_snippets", "snippets", package = PACKAGE_NAME)
  if (dir.exists(path)) {
    path
  } else {
    character(0)
  }
}

get_snippets_paths_git <- function() {
  Sys.glob(file.path(get_user_git_dir(),"*", "snippets"))
}

get_snippets_paths_imported_projects <- function() {
  file.path(get_imported_projects_dirs(), "snippets")
}

get_snippets_paths_current_project <- function() {
  file.path(get_user_project_dir(), "snippets")
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
    full_path = normalizePath(winslash = "/", list.files(path, recursive = TRUE, pattern = "\\.R$", full.names = TRUE)),
    short_path = list.files(path, recursive = TRUE, pattern = "\\.R$", full.names = FALSE),
    stringsAsFactors = FALSE
  )
}

find_snippets <- function(paths) {
  dfs <- lapply(paths, find_snippets_path)
  df <- do.call(dplyr::bind_rows, args = dfs)
  df <- df[!duplicated(df$short_path), ]
  df
}

merge_all_snippets <- function() {
  paths <- c(
    # The order of this list matters; if a file appears in the same path in multiple
    # sources, the latest one will take precedence
    get_snippets_paths_current_project(),
    get_snippets_paths_git(),
    get_snippets_paths_imported_projects(),
    get_snippets_paths_builtin()
  )
  find_snippets(paths)
}

get_editable_snippets_paths <- function() {
  git_paths <- get_writable_git_repos()
  paths <- list("Current Project" = get_user_project_dir())
  if (length(git_paths) > 0) {
    paths[["Git Repos"]] <- stats::setNames(git_paths, basename(git_paths))
  }
  paths
}

add_snippet <- function(contents, name, repo, local_folder) {
  snippet_file_name <- paste0(name, ".R")
  snippet_dir <- file.path(repo, "snippets", local_folder)
  if (!dir.exists(snippet_dir)) {
    dir.create(snippet_dir, showWarnings = FALSE, recursive = TRUE)
  }
  snippet_path <- file.path(snippet_dir, snippet_file_name)
  writeLines(contents, snippet_path)
  snippet_path
}

edit_snippet <- function(contents, file) {
  writeLines(contents, file)
}
