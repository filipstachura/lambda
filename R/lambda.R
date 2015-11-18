.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

`%=>%` <- function(...) {
  args <- as.list(match.call())
  lambdaArgs <- args[2][[1]]
  body <- args[3][[1]]

  rawArgs <- deparse(lambdaArgs)
  len <- length(rawArgs)
  rawArgs <- .trim(if(len > 1) rawArgs[2:(len - 1)] else rawArgs)
  lambda <- function(...) {
    parameters <- as.list(match.call())[-1]
    names(parameters) <- as.character(rawArgs)
    eval(body, envir=parameters)
  }
  lambda
}

`%$%` <- function(f, arg) {
  do.call(f, list(arg))
}
