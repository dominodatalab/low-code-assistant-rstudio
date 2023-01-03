httptest::with_mock_api({
  test_that("check_response works", {
    expect_error(check_response(httr::GET("https://httpbin.org/status/200")), NA)
    expect_error(check_response(httr::GET("https://httpbin.org/status/300")), "300")
    expect_error(check_response(httr::GET("https://httpbin.org/status/400")), "400")
  })
})

httptest::with_mock_api({
  test_that("parse_response works", {
    expect_error(parse_response(httr::GET("http://httpbin.org/json")), NA)
    expect_error(parse_response(httr::GET("http://httpbin.org/html")), "html")
    expect_error(parse_response(httr::GET("http://httpbin.org/xml")), "xml")

    parsed <- parse_response(httr::GET("http://httpbin.org/json"))
    expect_equal(length(parsed), 1L)
    expect_equal(length(parsed$slideshow), 4L)
  })
})

httptest::with_mock_api({
  test_that("call_api works", {
    expect_error(call_api("doesnotexist"))
    expect_error(call_api("/doesnotexist"))

    res_version <- call_api("/version")
    expect_equal(length(res_version), 6L)
    expect_equal(res_version$version, "1.2.3")
    expect_named(res_version, c("buildId", "buildUrl", "commitId", "commitUrl", "timestamp", "version"))

    res_self <- call_api("/v4/users/self")
    expect_equal(length(res_self), 7L)
    expect_equal(res_self$id, "123456abcdef")
    expect_named(res_self, c("firstName", "lastName", "fullName", "userName", "email", "avatarUrl", "id"))

  })
})
