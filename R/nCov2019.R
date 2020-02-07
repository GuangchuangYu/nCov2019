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

#' load historical data of wuhan 2019-Cov
#'
#' @title load_nCov2019
#' @return nCov2019History object
#' @export
#' @author Guangchuang Yu
load_nCov2019 <- function() {
  readRDS(system.file("nCov2019History.rds", package="nCov2019"))
}
