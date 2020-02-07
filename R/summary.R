##' @method summary nCov2019
##' @export
summary.nCov2019 <- function(object, by = "total", ...) {
  by <- match.arg(by, c("total", "today"))
  if (by == "total") {
    return(object$chinaDayList)
  }
  return(object$chinaDayAddList)
}

##' @method summary nCov2019History
##' @export
summary.nCov2019History <- function(object, province, ...) {
  obj <- object$data
  if (missing(province)) {
    province <- unique(obj$province)
  }
  if (is.numeric(province)) {
    province <- unique(obj$province)[province]
  }
  res <- obj[obj$city %in% province, ]
  res[,names(res) != 'city']
}
