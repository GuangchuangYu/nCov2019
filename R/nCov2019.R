#' download statistical numbers of the wuhan 2019-nCov 
#'
#' @title get_nCov2019
#' @return nCov2019 object
#' @export
#' @importFrom jsonlite fromJSON
#' @author Guangchuang Yu
get_nCov2019 <- function() {
  url = 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110'
  x = suppressWarnings(readLines(url, encoding="UTF-8"))
  x = sub("^\\d+\\(", "", x)
  x = sub("\\)$", "", x)
  y = jsonlite::fromJSON(x)
  d = jsonlite::fromJSON(y$data)
  
  class(d) <- 'nCov2019'
  return(d)
}

##' @method print nCov2019
##' @export
print.nCov2019 = function(x, ...) {
  cat("China (total confirmed cases):", x$chinaTotal$confirm)
  cat("\nlast update:", x$lastUpdateTime, "\n")
}

##' @method [ nCov2019
##' @export
`[.nCov2019` = function(object, i, j, ...) {
  d <- object$areaTree[1,2][[1]]
  name = d[[1]]
  if (missing(i)) {
    res <- cbind(name=name, d[[3]])
    res <- res[1:nrow(res),j, drop=F]
    return(res)
  } 
  
  if (is.character(i)) {
    i <- which(name == i)
  }
  stats <- d[i, 2][[1]]
  res <- cbind(name=stats$name, stats$total)
  res[1:nrow(res), j, drop=F]
}


##' @method summary nCov2019
##' @export
summary.nCov2019 = function(object, ...) {
  object$chinaDayList
}
