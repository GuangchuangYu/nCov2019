setup_province <- function(province) {
    province = sub("省", "", province)
    province = sub("自治区", "", province)
    province = sub("市", "", province)
    province = sub("特别行政区", "", province)
    province = sub("维吾尔", "", province)
    province = sub("壮族", "", province)
    province = sub("回族", "", province)
    return(province)
}

setup_city <- function(city) {
    city <- as.character(city)
    city <- sub("市$", "", city)
    city <- sub("地区$","", city)
    city <- sub("省$","", city)
    city <- sub("特别行政区$","", city)
    city <- sub("土家族","", city)
    city <- sub("蒙古族","", city)
    city <- sub("哈尼族","", city)
    city <- sub("布依族","", city)
    city <- gsub(".族","", city)
    city <- sub("自治","", city)
    city[city == '神农架林区'] = '神农架'
    city[city == '甘孜州'] = '甘孜'
    city[city == '凉山州'] = '凉山'
    return(city)
}

special_city <- c("上海", "北京", "天津",
                  "澳门", "重庆", "香港")


ncovEnv = new.env()
assign("setup_province", setup_province, envir = ncovEnv)
assign("setup_city", setup_city, envir = ncovEnv)
assign("special_city", special_city, envir = ncovEnv)

save(ncovEnv, file="ncovEnv.rda")