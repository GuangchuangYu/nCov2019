#' download statistical numbers of the wuhan 2019-nCov
#'
#' @title get_nCov2019
#' @param lang one of 'zh' and 'en', for setting language of province and city names.
#' If lang = "auto" (default), it will be set based on Sys.getlocale("LC_CTYPE")
#' @return nCov2019 object
#' @export
#' @importFrom jsonlite fromJSON
#' @author Guangchuang Yu
get_nCov2019 <- function(lang = 'auto') {
  lang <- which_lang(lang)
  data <- .get_qq_data()

  if (lang == 'en') {
    # change countries to English
    nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
    data$areaTree$name <- nn[as.character(data$areaTree$name)]

    # change provinces to English
    prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
    data$areaTree[[1,"children"]]$name <- trans_province(data$areaTree[[1, "children"]]$name)
    
    # change cities to English
    for (i in 1:nrow(data$areaTree[[1,"children"]])) {
      data$areaTree[[1, "children"]][[i, "children"]]$name <- trans_city(data$areaTree[[1, "children"]][[i, "children"]]$name)
    }
  }

  data$lang <- lang
  structure(data, class = 'nCov2019')
}

         
#' load historical data of wuhan 2019-Cov
#'
#' @title load_nCov2019
#' @inheritParams get_nCov2019
#' @param source historical data source, one of 'github' or 'dxy'
#' @return nCov2019History object
#' @importFrom downloader download
#' @export
#' @author Guangchuang Yu
load_nCov2019 <- function(lang = 'auto', source="github") {
  lang <- which_lang(lang)
  source <- match.arg(source, c("github", "dxy"))
  rds <- tempfile(pattern=".rds")
  if (source == 'dxy'){
    url = 'https://gitee.com/timze/historicaldata/raw/master/dxy_origin_historical_data.rds'
  } else { 
    url = 'https://gitee.com/timze/historicaldata/raw/master/dxy_historical_data.rds'
  }
  downloader::download(url,destfile = rds, quiet = TRUE)
  data <- readRDS(rds)
  ## data <- readRDS(system.file("nCov2019History.rds", package="nCov2019"))
  
  prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
  
  if (lang == 'en') {
    # change provinces and city columns to English; for x$data
    data$data$province <- trans_province(data$data$province)
    data$data$city <- trans_city(data$data$city)
    # change provinces to English; for x$province
    data$province$province <- trans_province(data$province$province)
    # change countries to English; for github source only 
    if (source == 'github'){
    nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
    data$global$country <-  nn[as.character(data$global$country)]
    }
  }
  data$lang <- lang
  return(data)
}

