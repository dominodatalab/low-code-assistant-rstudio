run_xform <- function(data, xform, tidyverse = TRUE) {
  eval(parse(text = xform$get_code("data")))
}

clean_search_path <- function(init) {
  sapply(setdiff(search(), init), function(name) detach(name, character.only = TRUE))
}

get_tidyverse_status <- function(xformseq) {
  sapply(xformseq$transformations, function(xform) xform$tidyverse)
}

get_xform_sel1 <- function() {
  SelectTransformation$new("col1")
}
get_xform_sel2 <- function() {
  SelectTransformation$new(c("col1", "col2"))
}
get_xform_drop1 <- function() {
  DropTransformation$new("col1")
}
get_xform_drop2 <- function() {
  DropTransformation$new(c("col1", "col2"))
}
get_xform_missing <- function() {
  MissingValuesTransformation$new()
}

is_shiny_exists <- function(driver, id) {
  driver$get_js(paste0("!!document.getElementById('", id, "')"))
}

is_shiny_enabled <- function(driver, id) {
  driver$get_js(paste0("!$('#", id, "').is(':disabled')"))
}

is_shiny_visible <- function(driver, id) {
  driver$get_js(paste0("$('#", id, "').is(':visible')"))
}
