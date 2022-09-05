xforms_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fontawesome::fa_html_dependency(),
    tags$head(tags$style(HTML(
      ".cell-filter-btn, .col-drop-btn { opacity: 0; cursor: pointer; transition: opacity 0.4s; }
     .rt-td:hover .cell-filter-btn, .rt-th:hover .col-drop-btn { display: inline-block; opacity: 0.5; }
     .rt-td .cell-filter-btn:hover, .rt-th .col-drop-btn:hover { opacity: 1; } "
    ))),
    reactable::reactableOutput(ns("table"))
  )
}

#' @param data (reactive) Data to show in the table
#' @return List:
#'   - drop: (reactive) A drop transformation that the user selected
#'   - missing: (reactive) A missing values transformation that the user selected
#'   - filter: (reactive) A filter transformation that the user selected (the 'type' isn't set correctly)
xforms_table_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      output$table <- reactable::renderReactable({
        req(data())

        if (ncol(data()) == 0) {
          return(reactable::reactable(data.frame("." = "Data has no columns")))
        }

        reactable::reactable(
          data(),
          compact = TRUE,
          showPageSizeOptions = TRUE,
          defaultPageSize = 10,
          pageSizeOptions = c(10, 25, 50, 100),
          pagination = TRUE,
          highlight = TRUE,
          rownames = TRUE,
          columns = list(
            .rownames = reactable::colDef(
              name = "#",
              width = 50,
              style = list(`font-style` = "italic"),
              headerStyle = list(`font-style` = "italic")
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left",
            na = "<span style='font-style: italic; opacity: 0.5;'>–</span>",
            cell = reactable::JS(
              "function(cellInfo) {
                 if (cellInfo.column.name == '#') {
                   return cellInfo.value;
                 }
                 return '<div>' + cellInfo.value + ' <i title=\"Filter values like this\" class=\"fa fa-filter cell-filter-btn\"></i></div>';
              }"
            ),
            header = function(value, col) {
              if (col == ".rownames") {
                value
              } else {
                onclick <- glue::glue(
                  "event.stopPropagation(); ",
                  "Shiny.setInputValue('{{session$ns('drop')}}', `${event.target.dataset.colName}`, {priority: 'event'});",
                  .open = "{{",
                  .close = "}}"
                )
                tags$div(
                  value, " ",
                  icon("trash-alt", class = "col-drop-btn", title = "Drop this column", `data-col-name` = value, onclick = onclick)
                )
              }
            },
            html = TRUE
          ),
          onClick = reactable::JS(
            glue::glue(
              "function(rowInfo, column) {
                 if (!event.target.classList.contains('cell-filter-btn')) {
                   return;
                 }
                 let value = rowInfo.row[column.name];
                 if (value === null || (value === 'NA' && column.type === 'numeric')) {
                   Shiny.setInputValue('{{session$ns('missing')}}', `${column.name}`, {priority: 'event'});
                 } else {
                   Shiny.setInputValue('{{session$ns('filter')}}', { col: `${column.name}`, val: `${value}`, missing: true }, {priority: 'event'});
                 }
               }",
              .open = "{{",
              .close = "}}"
            )
          )
        )
      })

      drop <- reactive({
        req(input$drop)
        DropTransformation$new(cols = input$drop)
      })
      missing <- reactive({
        req(input$missing)
        MissingValuesTransformation$new(col = input$missing)
      })
      filter <- reactive({
        req(input$filter)
        FilterTransformation$new(
          col = input$filter$col,
          op = FilterTransformation$OPTIONS$EQUALS,
          value = input$filter$val
        )
      })

      return(list(
        drop = drop,
        missing = missing,
        filter = filter
      ))

    }
  )
}
