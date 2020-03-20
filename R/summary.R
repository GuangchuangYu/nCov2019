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
  obj <- object$province
  # get the country setting
  country_option = getOption("nCov2019.country")
  nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
  # translate country setting to user language setting
  if (object$lang == 'zh'){
    country_option <- names(nn)[nn == country_option]
    }
  
  if (missing(province)) {
    province <- unique(obj$province)
  }
  if (is.numeric(province)) {
    province <- unique(obj$province)[province]
  }
  res <- obj[obj$province %in% province,]
  
  ## there is Jilin province and Jilin city, may caused some problems.
  ##
  # res <- group_by(res, time) %>% transform(cum_confirm = max(cum_confirm))
  subset(unique(res),country == country_option)
  #res[,names(res) != 'city']
}
