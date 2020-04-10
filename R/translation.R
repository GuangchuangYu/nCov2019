##' translate province from Chinese to English and vice versa
##'
##' @title trans_province
##' @param province province name
##' @param lang language of province name. if 'zh', translate to 'en' and if 'en', translate to 'zh'
##' @return translated province name
##' @export
##' @author Patrick Tung and Guangchuang Yu
trans_province <- function(province, lang="zh") {
  lang <- match.arg(lang, c("zh", "en"))
  prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
  oversea <- readRDS(system.file('oversea_province_translate.rds', package="nCov2019"))
  prov_cities <- rbind(prov_cities[,1:2],oversea)  
  if (lang == "zh") {
    load(system.file("ncovEnv.rda", package="nCov2019"))
    ncovEnv <- get("ncovEnv")
    setup_province <- get("setup_province", envir = ncovEnv)
    province <- setup_province(province)
    
    res <- prov_cities$province_name_en[match(province, prov_cities$province_name_zh)]
  } else {
    res <- prov_cities$province_name_zh[match(province, prov_cities$province_name_en)]
  }
  return(res)
}

##' translate city from Chinese to English and vice versa
##'
##' @title trans_city
##' @param city city name
##' @param lang language of city name. if 'zh', translate to 'en' and if 'en', translate to 'zh'
##' @return translated city name
##' @importFrom dplyr bind_rows
##' @export
##' @author Patrick Tung and Guangchuang Yu
trans_city <- function(city, lang="zh") {
  lang <- match.arg(lang, c("zh", "en"))
  prov_cities <- jsonlite::fromJSON(system.file('provinces_and_cities.json', package="nCov2019"))
  city_df <- unique(dplyr::bind_rows(prov_cities$cities))
  if (lang == "zh") {
    load(system.file("ncovEnv.rda", package="nCov2019"))
    ncovEnv <- get("ncovEnv")
    setup_city <- get("setup_city", envir = ncovEnv)
    
    city <- setup_city(city)
    res <- city_df$city_name_en[match(city, city_df$city_name_zh)]
  } else {
    res <- city_df$city_name_zh[match(city, city_df$city_name_en)]
  }
  return(res)
}

