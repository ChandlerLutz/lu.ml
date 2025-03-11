f_stopifnot <- function(condition, msg) 
  if (!all(condition)) stop(msg)

## https://stackoverflow.com/a/16275428
chunk2 <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
