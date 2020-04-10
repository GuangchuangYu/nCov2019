#' download statistical numbers of the COVID-2019
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
    # change global countries name to English
    data$global$name <- nn[as.character(data$global$name)]

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

         
#' load historical data of COVID-2019
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
  source <- match.arg(source, c("github", "cnnhc", "dxy"))
  rds <- tempfile(pattern=".rds")
  url1 = 'https://gitee.com/timze/historicaldata/raw/master/dxy_data.rds'
  url2 = 'https://gitee.com/timze/historicaldata/raw/master/nhc_data.rds'
  url3 = 'https://gitee.com/timze/historicaldata/raw/master/github_data.rds'
  url = ifelse(source == 'dxy',url1,
               ifelse(source == 'cnnhc', url2, 
               url3))
  data = tryCatch({
    downloader::download(url, destfile = rds, quiet = TRUE)
    readRDS(rds)
  }, error   = function(e) { 
    message("Historical data downloaded failed. \
    You will use locally stored data. \
    To get the latest data, please check your network connection or try again later.")
    readRDS(system.file("nCov2019History_3_11.rds", package="nCov2019"))[[source]]
  })
  ## data <- readRDS(system.file("nCov2019History.rds", package="nCov2019"))
  
  prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
  
  if (lang == 'en') {
    # change counry, province and city columns to English; for city level data
    nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
    data$data$country <- nn[as.character(data$data$country)]
    data$data$province <- trans_province(data$data$province)
    if (source != 'cnnhc'){
    data$data$city <- trans_city(data$data$city)
    }

    # change country and province columns to English; for province level data
    data$province$country <- nn[as.character(data$province$country)]
    data$province$province <- trans_province(data$province$province)
    
    # change country to English; for global level data
    if (source == 'github' | source == 'cnnhc'){
    data$global$country <-  nn[as.character(data$global$country)]
    }
  }
  data$lang <- lang
  return(data)
}
