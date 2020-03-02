##' @importFrom stats time
##' @method time nCov2019
##' @export
time.nCov2019 <- function(x, ...) {
  x$lastUpdateTime
}

##' @method time nCov2019History
##' @export
time.nCov2019History <- function(x, ...) {
  x$time
}

