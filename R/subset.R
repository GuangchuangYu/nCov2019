##' @method [ nCov2019History
##' @export
`[.nCov2019History` <- function(object, i, j, quiet = TRUE, ...) {
  obj <- object$data
  # get the country setting
  country_option = getOption("nCov2019.country")
  nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
  # translate country setting to user language setting
  if (object$lang == 'zh'){
    country_option <- names(nn)[nn == country_option]
    }
  # if missing i, then will return the city level data
  if (missing(i)) { i = "city"}

  # City level data only avalibale for China now 
  if (i == 'city') {
    if (country_option == 'China') {
      obj = subset(obj[, j, drop=FALSE],country == country_option)
      return(obj) 
    } else {
        if (!quiet) {
            msg <- paste("Only China have city-level information.\n",
                         paste0("Province-level information are available for specific countries \n\t(",
                                paste0(country_list[country_list != "China"], collapse=","),")\n"),
                         "You can also use x['global'] to access information at country-level\n")
            message(msg)            
        }
        i <- "province"
    }
  }
  # return province data base on the country_option
  if (i=='province'){
    return(subset(object$province, country == country_option)) 
  }
  if (i=='global'){
    return(object$global) 
  }
  if (is.numeric(i)) {
    i <- unique(obj$province)[i]
  }

  ## load(system.file("ncovEnv.rda", package="nCov2019"))
  ## ncovEnv <- get("ncovEnv")
  ## special_city <- get("special_city", envir = ncovEnv)

  # if i is a specified province
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
