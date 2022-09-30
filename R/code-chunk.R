code_chunk_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fontawesome::fa_html_dependency(),

    singleton(tags$head(tags$style(HTML(
      ".lca-code-chunks { padding: 0.8rem; background: #fafafa; border: 1px solid #ececec; }
       .lca-code-chunks pre { background: inherit; border: 0; padding: 0; margin: 0; }
       .lca-code-chunk { position: relative; padding: 0.2rem; border-radius: 4px; transition: background 0.25s; }
       .lca-code-chunk.chunk-editable:hover { background: #eee; }
       .lca-code-chunk .chunk-btns { position: absolute; top: 0; right: 0.2rem; color: #444; opacity: 0; transition: opacity 0.25s; font-size: 1.2em; }
       .lca-code-chunk:hover .chunk-btns { opacity: 1; }
       .lca-code-chunk .chunk-btn { opacity: 0.5; cursor: pointer; transition: opacity 0.25s; margin: 0 .3rem; }
       .lca-code-chunk .chunk-btn:hover { opacity: 1; }
       .lca-code-chunk.chunk-error { background: #f2dede; }
       .lca-code-chunk.chunk-error:hover { background: #f1d0d0; }
       .lca-code-chunk .chunk-error-icon { color: #d2322d; margin-left: 4px; }"
    )))),

    htmltools::htmlDependency(
      "highlight.js",
      "6.2",
      src = "www/shared/highlight",
      package = "shiny",
      script = "highlight.pack.js",
      stylesheet = "rstudio.css"
    ),

    uiOutput(ns("code_section"))
  )
}

code_chunk_server <- function(id, chunks = NULL, error_line = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      chunks_r <- make_reactive(chunks)
      error_line_r <- make_reactive(error_line)

      output$code_section <- renderUI({
        if (length(chunks_r()) == 0 || (length(chunks_r()) == 1 && chunks_r() == "")) {
          return()
        }

        chunks_html <- lapply(seq_along(chunks_r()), function(chunk_idx) {
          chunk <- chunks_r()[[chunk_idx]]
          edit <- (chunk_idx < length(chunks_r()))
          error <- (!is.null(error_line_r()) && error_line_r() == chunk_idx)

          if (error) {
            chunk <- paste0(
              chunk,
              "<i class='fa fa-bug chunk-error-icon'></i>"
            )
          }
          chunk_html <- tags$pre(HTML(as.character(tags$div(HTML(chunk), class = "language-r hl-me"))))

          onclick_tpl <- function(action) {
            glue::glue(
              "Shiny.setInputValue('{{session$ns(action)}}', `${event.target.closest('.chunk-btns').dataset.chunkNum}`, {priority: 'event'});",
              .open = "{{", .close = "}}"
            )
          }

          tags$div(
            chunk_html,
            if (edit) tags$div(
              class = "chunk-btns",
              `data-chunk-num` = chunk_idx,
              tags$i(
                title = "Insert before this line",
                class = "fa fa-share chunk-btn fa-flip-horizontal fa-fw",
                onclick = onclick_tpl("insert")
              ),
              tags$i(
                title = "Modify",
                class = "fa fa-pen chunk-btn fa-fw",
                onclick = onclick_tpl("modify")
              ),
              tags$i(
                title = "Delete",
                class = "fa fa-trash-alt chunk-btn fa-fw",
                onclick = onclick_tpl("delete")
              )
            ),
            class = "lca-code-chunk",
            class = if (edit) "chunk-editable",
            class = if (error) "chunk-error"
          )
        })

        tagList(
          tags$div(class = "lca-code-chunks", chunks_html),
          HTML(as.character(tags$script(
            'setTimeout(function() {
               document.querySelectorAll(".lca-code-chunks .hl-me").forEach(function(el) { hljs.highlightBlock(el); })
            }, 0);'
          )))
        )
      })

      return(list(
        insert = reactive(req(as.integer(input$insert))),
        modify = reactive(req(as.integer(input$modify))),
        delete = reactive(req(as.integer(input$delete)))
      ))

    }
  )
}
