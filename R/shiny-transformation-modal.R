transformation_modal <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data <- reactiveValEvent(NULL)
      action <- reactiveValEvent(NULL)
      xform <- reactiveValEvent(NULL)
      meta <- reactiveValEvent(NULL)

      result_xform <- reactiveVal(NULL)

      dialog <-
        modalDialog(
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("apply"), "Apply", class = "btn-primary"),
            modalButton("Cancel")
          ),
          shinyjs::useShinyjs(),
          h2(div(id = ns("title"))),
          fluidRow(
            column(
              12,
              selectInput(
                ns("xform_type"), label = NULL,
                choices = setNames(
                  lapply(ALL_XFORMS, function(x) x$name_short),
                  lapply(ALL_XFORMS, function(x) x$name_long)
                )
              )
            )
          ),
          lapply(ALL_XFORMS, function(x) {
            conditionalPanel(
              glue::glue("input.xform_type == '{x$name_short}'"),
              ns = ns,
              x$shiny$ui(ns(x$name_short))
            )
          })
        )

      xform_modules <- lapply(ALL_XFORMS, function(x) {
        x$shiny$server(x$name_short, data = data, old_xform = xform)
      })
      xform_modules <- setNames(xform_modules, lapply(ALL_XFORMS, .subset2, "name_short"))

      show <- function(data, action = c("add", "insert", "edit"), xform = NULL, meta = NULL) {
        action <- match.arg(action)

        if (action == "edit" && is.null(xform)) {
          stop("Must provide a transformation for edit", call. = FALSE)
        }

        showModal(dialog)

        data(data)
        action(action)
        xform(xform)
        meta(meta)
      }

      observeEvent(action(), {
        shinyjs::html("title", paste0(firstup(action()), " transformation"))
      })

      observeEvent(xform(), {
        if (!is.null(xform())) {
          shinyjs::hide("xform_type")
          lapply(ALL_XFORMS, function(x) {
            if (inherits(xform(), x$classname)) {
              updateSelectInput(session, "xform_type", selected = x$name_short)
            }
          })
        }
      })

      validated <- reactive({
        req(input$xform_type)
        xform_modules[[input$xform_type]]$validate()
      })

      observe({
        shinyjs::toggleState("apply", condition = validated())
      })

      observeEvent(input$apply, {
        action <- xform_modules[[input$xform_type]]$xform()
        result_xform(action)
        removeModal()
      })

      return(
        list(
          result = result_xform,
          show = show,
          action = action,
          meta = meta
        )
      )
    }
  )
}
