snippet_edit_mode_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(force = TRUE),
    waiter::useWaiter(),

    shinyjs::hidden(
      shinytip::tip(
        div(
          class = "action-button snippet-edit-mode-btn col-grey",
          id = ns("enable_edit"),
          span(
            class = "fa-stack",
            icon("pen", class = "fa-stack-1x"),
            icon("slash", class = "fa-stack-1x")
          )
        ),
        "Snippet editing disabled. Click to enable",
        position = "right"
      ),
      shinytip::tip(
        div(
          class = "action-button snippet-edit-mode-btn col-link",
          id = ns("disable_edit"),
          span(
            class = "fa-stack",
            icon("pen", class = "fa-stack-1x")
          )
        ),
        "Snippet editing enabled. Click to disable",
        position = "right"
      ),
      div(
        id = ns("editing_btns"),
        actionButton(
          ns("add"),
          "Add",
          icon = icon("plus"),
          class = "btn-primary btn-lg"
        ),
        shinyjs::hidden(
          actionButton(
            ns("edit"),
            "Edit",
            icon = icon("pencil"),
            class = "btn-primary btn-lg"
          )
        ),
        shinyjs::hidden(
          actionButton(
            ns("delete"),
            "Delete",
            icon = icon("trash"),
            class = "btn-primary btn-lg"
          )
        )
      )
    )
  )
}

snippet_edit_mode_server <- function(id, selected_snippet, selected_snippet_full, curwd,
                                     snippets_refresh = reactive_trigger()) {

  moduleServer(
    id,
    function(input, output, session) {

      init_edit_mode <- .globals$snippets$edit %||% FALSE

      edit_mode <- reactiveVal(init_edit_mode)

      observeEvent(edit_mode(), {
        .globals$snippets$edit <- edit_mode()
        shinyjs::toggle("enable_edit", condition = !edit_mode())
        shinyjs::toggle("disable_edit", condition = edit_mode())
        shinyjs::toggle("editing_btns", condition = edit_mode())
      })

      observeEvent(input$enable_edit, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_SNIPPETS,
            type = "enable edit"
          )
        )

        edit_mode(TRUE)
      })
      observeEvent(input$disable_edit, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_SNIPPETS,
            type = "disable edit"
          )
        )

        edit_mode(FALSE)
      })

      editable_paths_list <- reactive({
        waiter::waiter_show(color = "#333e48cc", html = waiter::spin_plus())
        on.exit(waiter::waiter_hide(), add = TRUE)
        get_editable_snippets_paths()
      })

      can_edit_file <- reactive({
        if (is.null(selected_snippet()) || !edit_mode()) {
          return(FALSE)
        }
        editable_paths <- unname(unlist(editable_paths_list()))
        any(sapply(editable_paths, is_subdir, selected_snippet_full()))
      })

      observe({
        shinyjs::toggle("edit", condition = !is.null(selected_snippet()))
        shinyjs::toggle("delete", condition = !is.null(selected_snippet()))
      })

      observe({
        shinyjs::toggleState("edit", condition = can_edit_file())
        shinyjs::toggleState("delete", condition = can_edit_file())
      })

      add_edit_modal <- snippet_add_edit_modal("snippet_modal", editable_paths = editable_paths_list)

      observeEvent(input$add, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_SNIPPETS,
            type = "add"
          )
        )

        add_edit_modal$show(action = "add", folder = curwd())
      })

      observeEvent(input$edit, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_SNIPPETS,
            type = "edit"
          )
        )

        add_edit_modal$show(action = "edit", folder = curwd(), snippet = selected_snippet_full())
      })

      observeEvent(add_edit_modal$update(), {
        snippets_refresh$trigger()
      })

      observeEvent(input$delete, {
        shinymixpanel::mp_track(
          MIXPANEL_EVENT_INTERACTION,
          list(
            section = MIXPANEL_SECTION_SNIPPETS,
            type = "delete"
          )
        )

        shinyalert::shinyalert(
          title = "Are you sure you want to delete this snippet?",
          text = get_file_name_no_ext(selected_snippet()),
          showCancelButton = TRUE, cancelButtonText = "Cancel", confirmButtonText = "Delete",
          inputId = "delete_confirm", closeOnClickOutside = TRUE,
          confirmButtonCol = "#337ab7"
        )
      })

      observeEvent(input$delete_confirm, {
        if (!input$delete_confirm) {
          return()
        }
        tryCatch({
          file.remove(selected_snippet_full())
          snippets_refresh$trigger()
          shinyalert::shinyalert(type = "success", "Snippet deleted", confirmButtonCol = "#337ab7")
        }, error = function(err) {
          shinyalert::shinyalert(type = "error", title = "Error", text = err$message, confirmButtonCol = "#337ab7")
        })
      })

    }
  )
}
