library(shiny)

ui <- fluidPage(
  htmltools::htmlDependency(
    "highlight.js",
    "6.2",
    src = "www/shared/highlight",
    package="shiny",
    script = "highlight.pack.js",
    stylesheet = "rstudio.css",
    head = '<script>setTimeout(function() {hljs.highlightBlock(document.getElementById("ff"));}, 10)</script>'
  ),
  div(pre(HTML(as.character(tags$div(id="ff",class = "language-r", "library(yy)\nlibrary(hh)"))))),
  HTML(as.character(tags$script(
    ' setTimeout(function() {hljs.highlightBlock(document.getElementById("ff"));}, 10)'
  )))
)

server <- function(input, output, session) {
}

shinyApp(ui, server)

# each code chunk should be wrapped in a div so that actions can be taken per code chunk
# highlight should be called on each code chunk
code_chunk_server <- function(chunks, error_line = 0, insert = TRUE, modify = TRUE, delete = TRUE) {

}
