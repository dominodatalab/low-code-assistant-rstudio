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

is_git_writable <- function(dir) {
  tryCatch({
    owd <- setwd(dir)
    on.exit(setwd(owd), add = TRUE)
    gitcreds::gitcreds_get(use_cache = FALSE, set_cache = FALSE)
    processx::run("git", c("push", "--dry-run", "--force", "--no-verify"))
    TRUE
  }, error = function(err) FALSE)
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
