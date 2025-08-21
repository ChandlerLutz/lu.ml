f_stopifnot <- function(condition, msg) 
  if (!all(condition)) stop(msg)
