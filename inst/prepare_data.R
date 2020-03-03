x <- read.csv("nCov2019-data.csv", stringsAsFactors = F)
names(x)
head(x, 2)

colnames(x)[1:6] <- c("province", "city", "time", 
                      "cum_confirm", "cum_heal", "cum_dead")
colnames(x)[9:11] <- c("confirm", "dead", "heal")

#x$time = as.Date(sub("(\\d+)\\D+(\\d+)\\D+", "\\1-\\2", 
#                   x$time), "%m-%d")
x$time <- as.Date(x$time, "%Y/%m/%d")
head(x$time)

x$province <- gsub("\\s+", "", x$province)
which(grepl("\\s+", x$province))
head(x$province)
#x$province <- sub("\\s*$", "", x$province)
object <- structure(list(data = x,
                         time = x$time[1]
                         ), class = "nCov2019History")
saveRDS(object, file="nCov2019History.rds")
object


################# province data

txt <- "'山东':[117.000923, 36.675807],
'河北':[115.48333,38.03333],
'吉林':[125.35000,43.88333],
'黑龙江':[127.63333,47.75000],
'辽宁':[123.38333,41.80000],
'内蒙古':[111.670801, 41.818311],
'新疆':[87.68333,43.76667],
'甘肃':[103.73333,36.03333],
'宁夏':[106.26667,37.46667],
'山西':[112.53333,37.86667],
'陕西':[108.95000,34.26667],
'河南':[113.65000,34.76667],
'安徽':[117.283042, 31.86119],
'江苏':[119.78333,32.05000],
'浙江':[120.20000,30.26667],
'福建':[118.30000,26.08333],
'广东':[113.23333,23.16667],
'江西':[115.90000,28.68333],
'海南':[110.35000,20.01667],
'广西':[108.320004, 22.82402],
'贵州':[106.71667,26.56667],
'湖南':[113.00000,28.21667],
'湖北':[114.298572, 30.584355],
'四川':[104.06667,30.66667],
'云南':[102.73333,25.05000],
'西藏':[91.00000,30.60000],
'青海':[96.75000,36.56667],
'天津':[117.20000,39.13333],
'上海':[121.55333,31.20000],
'重庆':[106.45000, 29.56667],
'北京': [116.41667,39.91667],
'台湾': [121.30, 25.03],
'香港': [114.10000,22.20000],
'澳门': [113.50000,22.20000]"

pro <- readLines(textConnection(txt))

prov.df <- data.frame(
  name = sub("\\'(.*)\\'.*", "\\1", pro),
  long = as.numeric(sub(".*\\[([0-9\\.]+),.*", "\\1", pro)),
  lat = as.numeric(sub(".*,\\s*([0-9\\.]+)\\].*", "\\1", pro))
)
head(prov.df)
saveRDS(prov.df, file = "prov_location.rds")


####################### country data

country_zh_en = read.csv('country_zh_en.csv',stringsAsFactors = F)
nn = gsub(" $", "", country_zh_en[,2])
names(nn) = gsub(" $", "", country_zh_en[,1])
nn['阿联酋']
nn <- c(nn, '钻石号邮轮'='Diamond Princess',
        '日本本土'='Japan')
saveRDS(nn, file="country_translate.rds")
