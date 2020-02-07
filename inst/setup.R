setup_chinamap <- function(cn) {
    cn$province = sub("省", "", cn$province)
    cn$province = sub("自治区", "", cn$province)
    cn$province = sub("市", "", cn$province)
    cn$province = sub("特别行政区", "", cn$province)
    cn$province = sub("维吾尔", "", cn$province)
    cn$province = sub("壮族", "", cn$province)
    cn$province = sub("回族", "", cn$province)
    return(cn)
}

setup_city <- function(map) {
    map <- tibble::as_tibble(m)
    map$NAME <- as.character(map$NAME)
    map$NAME <- sub("市$", "", map$NAME)
    map$NAME <- sub("地区$","", map$NAME)
    map$NAME <- sub("省$","", map$NAME)
    map$NAME <- sub("特别行政区$","", map$NAME)
    map$NAME <- sub("土家族","", map$NAME)
    map$NAME <- sub("蒙古族","", map$NAME)
    map$NAME <- sub("哈尼族","", map$NAME)
    map$NAME <- sub("布依族","", map$NAME)
    map$NAME <- gsub(".族","", map$NAME)
    map$NAME <- sub("自治","", map$NAME)
    map$NAME[map$NAME == '神农架林区'] = '神农架'
    map$NAME[map$NAME == '甘孜州'] = '甘孜'
    map$NAME[map$NAME == '凉山州'] = '凉山'
    return(map)
}

special_city <- c("上海", "北京", "天津",
                  "澳门", "重庆", "香港")


ncovEnv = new.env()
assign("setup_chinamap", setup_chinamap, envir = ncovEnv)
assign("setup_city", setup_city, envir = ncovEnv)
assign("special_city", special_city, envir = ncovEnv)

save(ncovEnv, file="ncovEnv.rda")