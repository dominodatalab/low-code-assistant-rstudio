test_that("title_bar shiny ui", {
  expect_snapshot(title_bar_ui("test"))
  expect_snapshot(title_bar_ui("test", "page title"))
})

test_that("title_bar shiny server", {
  shiny::testServer(title_bar_server, {
    expect_null(title_r())
    expect_equal(output$page_title, "")
  })

  shiny::testServer(title_bar_server, args = list(title = "my title"), {
    expect_equal(title_r(), "my title")
    expect_equal(output$page_title, "my title")
  })

  title_var <- reactiveVal(NULL)
  shiny::testServer(title_bar_server, args = list(title = title_var), {
    expect_null(title_r())
    expect_equal(output$page_title, "")

    title_var("title1")
    session$flushReact()
    expect_equal(title_r(), "title1")
    expect_equal(output$page_title, "title1")

    title_var("title2")
    session$flushReact()
    expect_equal(title_r(), "title2")
    expect_equal(output$page_title, "title2")
  })
})
