#' download statistical numbers of the wuhan 2019-nCov
#'
#' @title get_nCov2019
#' @param lang one of 'zh' and 'en', for setting language of province and city names
#' @return nCov2019 object
#' @export
#' @importFrom jsonlite fromJSON
#' @author Guangchuang Yu
get_nCov2019 <- function(lang = 'zh') {
  lang <- match.arg(lang, c("zh", "en"))
  data <- jsonlite::fromJSON(.get_json())

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
#' @param lang one of 'zh' and 'en', for setting language of province and city names
#' @return nCov2019History object
#' @importFrom downloader download
#' @export
#' @author Guangchuang Yu
load_nCov2019 <- function(lang = 'zh') {
  lang <- match.arg(lang, c("zh", "en"))
  rds <- tempfile(pattern=".rds")
  downloader::download('https://gitee.com/timze/historicaldata/raw/master/historical_data.rds',
                destfile = rds, quiet = TRUE)
  data <- readRDS(rds)
  ## data <- readRDS(system.file("nCov2019History.rds", package="nCov2019"))
  
  prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
  
  if (lang == 'en') {
    # change provinces to English
    data$data$province <- trans_province(data$data$province)
    data$data$city <- trans_city(data$data$city)
  }
  data$lang <- lang
  return(data)
}

