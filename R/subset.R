##' @method [ nCov2019History
##' @export
`[.nCov2019History` <- function(object, i, j, ...) {
  obj <- object$data
  if (missing(i)) {
    return(obj[, j, drop=FALSE]) 
  }
  if (is.numeric(i)) {
    i <- unique(obj$province)[i]
  }

  ## load(system.file("ncovEnv.rda", package="nCov2019"))
  ## ncovEnv <- get("ncovEnv")
  ## special_city <- get("special_city", envir = ncovEnv)

  ii <- obj$province %in% i 

  obj[ii, j, drop=FALSE]
}


##' @method [ nCov2019
##' @export
`[.nCov2019` <- function(object, i, j, by="total", ...) {
  by <- match.arg(by, c("total", "today"))
  d <- object$areaTree[1, "children"][[1]]
  name = d[[1]]
  if (missing(i)) {
    res <- cbind(name=name, d[[by]])
  } else if (length(i) == 1) {
    res <- extract_province(object, i, by)
  } else {
    res <- do.call("rbind",
                   lapply(i, function(ii) {
                     extract_province(object, ii, by)
                   })
    )
  }
  
  res[1:nrow(res), j, drop=FALSE]
}
