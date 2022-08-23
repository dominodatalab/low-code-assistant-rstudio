`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}

cat0 <- function(...) {
  cat(..., sep = "")
}

firstup <- function(s) {
  substr(s, 1, 1) <- toupper(substr(s, 1, 1))
  s
}

isValidName <- function(s) {
  make.names(s) == s
}
