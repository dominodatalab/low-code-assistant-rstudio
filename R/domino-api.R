call_api <- function(endpoint, params = list()) {
  api_key <- get_api_key()

  auth <- httr::add_headers("X-Domino-Api-Key" = api_key)
  agent <- httr::user_agent(sprintf("LCA-R/%s", utils::packageVersion(PACKAGE_NAME)))
  timeout <- httr::timeout(20)
  url <- paste0(get_api_base(), endpoint)

  tryCatch({
    url <- httr::modify_url(url, query = params)
    response <- httr::GET(url, auth, agent, timeout)
  }, error = function(err) {
    stop("Error calling Domino API `", url, "`: ", err$message, call. = FALSE)
  })

  check_response(response)

  parse_response(response)
}

check_response <- function(res) {
  code <- httr::status_code(res)
  if (code != 200) {
    stop("Error with Domino API `", res$url, "`: response status: ", code, call. = FALSE)
  }
}

parse_response <- function(res) {
  if (httr::http_type(res) != "application/json") {
    stop("Domino API `", res$url, "`: did not return json (", httr::http_type(res), ")", call. = FALSE)
  }
  tryCatch({
    parsed <- httr::content(res, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(parsed, simplifyVector = FALSE)
    parsed
  }, error = function(err) {
    stop("Error parsing Domino API response `", res$url, "`: ", err$message, call. = FALSE)
  })
}
