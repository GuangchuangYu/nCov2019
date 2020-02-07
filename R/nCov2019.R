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
    nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
    data$areaTree$name <- nn[as.character(data$areaTree$name)]

    # change provinces to English
    prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
    data$areaTree[[1,"children"]] <- transform(data$areaTree[[1,"children"]],
                                      name = prov_cities$province_name_en[match(name, prov_cities$province_name_zh)])

    # change cities to English
    for (i in c(1:nrow(data$areaTree[[1,"children"]]))) {
      prov_name <- data$areaTree[[1,"children"]]$name[i]
      temp_cities <- dplyr::filter(prov_cities, province_name_en==prov_name)$cities[[1]]

      data$areaTree[[1,"children"]][[i,"children"]] <- transform(data$areaTree[[1,"children"]][[i,"children"]],
                                               name = temp_cities$city_name_en[match(name, temp_cities$city_name_zh)])
    }
  }

  data$lang <- lang
  structure(data, class = 'nCov2019')
}

#' load historical data of wuhan 2019-Cov
#'
#' @title load_nCov2019
#' @return nCov2019History object
#' @export
#' @author Guangchuang Yu
load_nCov2019 <- function(lang = 'zh') {
  data <- readRDS(system.file("nCov2019History.rds", package="nCov2019"))
  
  prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
  
  if (lang == 'en') {
    # change provinces to English
    data$data = transform(data$data, province = prov_cities$province_name_en[match(province, prov_cities$province_name_zh)])
    
    # change cities to English
    for (i in c(1:nrow(data$data))){
      prov_name <- data$data$province[i]
      temp_cities <- dplyr::filter(prov_cities, province_name_en==prov_name)$cities[[1]]
      data$data[i, ] <- transform(data$data[i, ], city = temp_cities$city_name_en[match(city, temp_cities$city_name_zh)])
    }
  }
  
  return(data)
}
