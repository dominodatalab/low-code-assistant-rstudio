# Modified from github.com/gerrymanoim/humanize

human_powers <- 10^c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 100)
names(human_powers) <- c(
  'thousand', 'million','billion','trillion',
  'quadrillion', 'quintillion','sextillion','septillion',
  'octillion','nonillion','decillion','googol'
)
natural_num <- function(value) {
  fmt <- '%.1f'
  if (value < human_powers[1]) {
    return(format(value, scientific = FALSE))
  }
  # Drop the first value, but shift our index
  for (i in seq_along(human_powers[-1]) + 1) {
    if (value < human_powers[[i]]) {
      chopped <- value / human_powers[[i - 1]]
      human_name <- names(human_powers[i - 1])
      return(paste(sprintf(fmt, chopped), human_name))
    }
  }
  # gigantic values
  return(format(value, scientific = FALSE))  # nocov
}


suffix <- c('kB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB')
natural_size <- function(bytes) {
  fmt <- '%.1f'

  base <- 1000

  if (bytes == 1) {
    return("1 Byte")
  } else if (bytes < base) {
    return(glue::glue("{bytes} Bytes"))
  }

  for (i in seq_along(suffix)) {
    unit <- base ^ (i + 1)
    if (bytes < unit) {
      out_val <- sprintf(fmt, (base * bytes / unit))
      return(glue::glue("{out_val} {suffix[[i]]}"))
    }
  }

  out_val <- sprintf(fmt, (base * bytes / unit))   # nocov
  return(glue::glue("{out_val} {suffix[[length(suffix)]]}"))   # nocov
}
