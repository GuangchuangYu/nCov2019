##' @method open nCov2019
##' @export
open.nCov2019 <- function(con, ...) {
  url <- 'https://news.qq.com/zt2020/page/feiyan.htm'
  utils::browseURL(url)
  invisible(url)
}
