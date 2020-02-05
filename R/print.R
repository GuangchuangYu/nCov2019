##' @method print nCov2019
##' @export
print.nCov2019 <- function(x, ...) {
  cat("China (total confirmed cases):", x$chinaTotal$confirm)
  cat("\nlast update:", time(x), "\n")
}

##' @method print nCov2019History
##' @export
print.nCov2019History <- function(x, ...) {
  cat("nCov2019 historical data", 
      "\nlast update:", as.character(time(x)), "\n")  
}

