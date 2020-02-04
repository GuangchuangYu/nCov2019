#' download statistical numbers of the wuhan 2019-nCov 
#'
#' @title get_nCov2019
#' @return nCov2019 object
#' @export
#' @importFrom jsonlite fromJSON
#' @author Guangchuang Yu
get_nCov2019 <- function() {
  structure(jsonlite::fromJSON(.get_json()),
            class = 'nCov2019')
}

#' load historical data of wuhan 2019-Cov
#' 
#' @title load_nCov2019
#' @return nCov2019History object
#' @export
#' @author Guangchuang Yu
load_nCov2019 <- function() {
  readRDS(system.file("nCov2019History.rds", package="nCov2019"))
}

