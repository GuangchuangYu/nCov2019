#' download statistical numbers of the wuhan 2019-nCov 
#'
#' @title get_nCov2019
#' @return nCov2019 object
#' @export
#' @importFrom jsonlite fromJSON
#' @author Guangchuang Yu
get_nCov2019 <- function(lang = 'zh') {
  data <- jsonlite::fromJSON(.get_json())
  
  if (lang == 'en') {
    # change countries to English
    countriesurl <- jsonlite::fromJSON('https://gist.githubusercontent.com/jacobbubu/060d84c2bdf005d412db/raw/845c78f55e49fee89814bdc599355069f07b7ee6/countries.json')
    countries <- countriesurl[, c('English', 'China')]
    countries$China <-gsub('；.*','',countries$China)
    data$areaTree <- transform(data$areaTree, name = countries$English[match(name, countries$China)])
    
    # change provinces to English
    prov_cities <- jsonlite::fromJSON('https://raw.githubusercontent.com/tungpatrick/nCov2019_prov_city_json/master/provinces_and_cities.json')
    data$areaTree[[1,4]] <- transform(data$areaTree[[1,4]], 
                                      name = prov_cities$province_name_en[match(name, prov_cities$province_name_zh)])
    
    # change cities to English
    for (i in c(1:nrow(data$areaTree[[1,4]]))) {
      prov_name <- data$areaTree[[1,4]]$name[i]
      temp_cities <- dplyr::filter(prov_cities, province_name_en==prov_name)$cities[[1]]
      
      data$areaTree[[1,4]][[i,4]] <- transform(data$areaTree[[1,4]][[i,4]], 
                                               name = temp_cities$city_name_en[match(name, temp_cities$city_name_zh)])
    }
  }
  
  structure(data, class = 'nCov2019')
}

##' @method print nCov2019
##' @export
print.nCov2019 <- function(x, ...) {
  cat("China (total confirmed cases):", x$chinaTotal$confirm)
  cat("\nlast update:", time(x), "\n")
}

##' @importFrom stats time
##' @method time nCov2019
##' @export
time.nCov2019 <- function(x, ...) {
  x$lastUpdateTime
}

##' @method open nCov2019
##' @export
open.nCov2019 <- function(con, ...) {
  url <- 'https://news.qq.com/zt2020/page/feiyan.htm'
  utils::browseURL(url)
  invisible(url)
}

##' @method [ nCov2019
##' @export
`[.nCov2019` <- function(object, i, j, by="total", ...) {
  by <- match.arg(by, c("total", "today"))
  d <- object$areaTree[1,4][[1]]
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
    
  res[1:nrow(res), j, drop=F]
}

##' @method summary nCov2019
##' @export
summary.nCov2019 <- function(object, by = "total", ...) {
  by <- match.arg(by, c("total", "today"))
  if (by == "total") {
    return(object$chinaDayList)
  }
  return(object$chinaDayAddList)
}


extract_province <- function(object, i, by) {
  if (i == 'global') {
    res <- cbind(name = object$areaTree[[1]], object$areaTree[[by]])
    return(res)
  } 
  
  d <- object$areaTree[1,2][[1]]
  name = d[[1]]
  if (is.character(i)) {
    i <- which(name == i)
  }
  stats <- d[i, 2][[1]]
  cbind(name=stats$name, stats[[by]])
}

.get_json <- function() {
  url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110'
  x <- suppressWarnings(readLines(url, encoding="UTF-8"))
  x <- sub("^\\d+\\(", "", x)
  x <- sub("\\)$", "", x)
  y <- jsonlite::fromJSON(x)
  return(y$data)
}

