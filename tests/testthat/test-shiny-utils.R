library(shiny)

test_that("reactiveValEvent invalidates every time, even if the value hasn't changed", {
  server <- function(input, output, session) {
    myval_event <- reactiveValEvent(NULL)
    myval_old <- reactiveVal(NULL)
    num_event <- reactiveVal(0)
    num_old <- reactiveVal(0)

    observeEvent(input$set, {
      myval_event(input$num)
      myval_old(input$num)
    })
    observeEvent(myval_event(), {
      num_event(num_event() + 1)
    })
    observeEvent(myval_old(), {
      num_old(num_old() + 1)
    })
    output$out_event <- renderText({
      num_event()
    })
    output$out_old <- renderText({
      num_old()
    })
  }

  testServer(server, {
    session$setInputs("num" = 10, "set" = 1)
    expect_equal(num_event(), 1)
    expect_equal(num_old(), 1)
    session$setInputs("num" = 10, "set" = 2)
    expect_equal(num_event(), 2)
    expect_equal(num_old(), 1)
    session$setInputs("num" = 10, "set" = 3)
    expect_equal(num_event(), 3)
    expect_equal(num_old(), 1)
    session$setInputs("num" = 9, "set" = 4)
    expect_equal(num_event(), 4)
    expect_equal(num_old(), 2)
    session$setInputs("num" = 8, "set" = 5)
    expect_equal(num_event(), 5)
    expect_equal(num_old(), 3)
    session$setInputs("num" = 8, "set" = 6)
    expect_equal(num_event(), 6)
    expect_equal(num_old(), 3)
  })
})

test_that("reactive_trigger works", {
   server <- function(input, output, session) {
    rxt <- reactive_trigger()
    num <- reactiveVal(0)
    observeEvent(input$go1, {
      rxt$trigger()
    })
    observeEvent(input$go2, {
      rxt$trigger()
    })
    observeEvent(rxt$depend(), {
      num(num() + 1)
    })
  }

   testServer(server, {
     session$setInputs("go1" = 1)
     expect_equal(num(), 1)
     session$setInputs("go1" = 2)
     expect_equal(num(), 2)
     session$setInputs("go1" = 3)
     expect_equal(num(), 3)
     session$setInputs("go2" = 1)
     expect_equal(num(), 4)
     session$setInputs("go2" = 2)
     expect_equal(num(), 5)
   })
})

test_that("input_with_checkbox works", {
  expect_error(input_with_checkbox("notag", checkboxId = "chx", checkboxLabel = "lbl"), "shiny.tag")
  expect_error(input_with_checkbox(div(), checkboxId = "chx", checkboxLabel = "lbl"), "<input> tag")
  expect_error(input_with_checkbox(div("noinput"), checkboxId = "chx", checkboxLabel = "lbl"), "<input> tag")
  expect_error(input_with_checkbox(div(div("f")), checkboxId = "chx", checkboxLabel = "lbl"), "<input> tag")
  expect_error(input_with_checkbox(textInput("x", "x", "x"), checkboxId = "chx", checkboxLabel = "lbl"), NA)
  expect_error(input_with_checkbox(textInput("x", "x", "x"), checkboxId = "chx", checkboxLabel = "lbl", side = "left"), NA)
  expect_error(input_with_checkbox(textInput("x", "x", "x"), checkboxId = "chx", checkboxLabel = "lbl", side = "right"), NA)
  expect_error(input_with_checkbox(textInput("x", "x", "x"), checkboxId = "chx", checkboxLabel = "lbl", side = "up"))

  expect_snapshot(input_with_checkbox(textInput("x", "x", "x"), "chx", "lbl"))
  expect_snapshot(input_with_checkbox(textInput("x", "x", "x"), "chx", "lbl", checkboxValue = TRUE))
  expect_snapshot(input_with_checkbox(textInput("x", "x", "x"), "chx", "lbl", side = "right"))
})
