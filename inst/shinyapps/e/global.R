# Shiny app for showing the number of confirmed coronavirus cases across China
# Xijin Ge 2/5/2020
#install.packages("dplyr","ggplot2","shiny")   # already included with rocker/shiny-verse docker image
#install.packages(c("forcats","ggrepel","forecast","plotly","shinyBS","lubridate"))
# install.packages("remotes")
# remotes::install_github("GuangchuangYu/nCov2019")  # main data package
# remotes::install_github("GuangchuangYu/chinamap")   #Chinese map
# install.packages(c("sp","mapproj","maps","sf"))
# install.packages("pinyin")


plotWidth = 800

#--------------English version or Chinese version
if (file.exists("English_version"))
isEnglish <- TRUE else isEnglish <- FALSE
#-----------------------------------------------
if(isEnglish)
  myURL = "http://www.bcloud.org/v/"  else
    myURL = "http://www.bcloud.org/e/" 

library(pinyin)
mypy <- pydic(method = "toneless",dic = 'pinyin2') # 载入默认字典
  #py("武汉", sep = "", dic = mypy) # 转换

py1 <- function (ChineseName) 
  # Translate Chinese names into English based on the pinyin package
  # The capitalize function is based the capitalize function in Hmsic package
  # ""武汉"" --> "Wuhan"
{
  if(!isEnglish) { 
    return(ChineseName) 
    } else {  
  string <- as.character( py(ChineseName, sep = "", dic = mypy) )
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
  }
}

py2 <- function (ChineseNames) 
  # Translate a vector of Chinese strings into English
  #  c("武汉", "上海") --> c("Wuhan", "Shanghai")
{
  if(!isEnglish) { 
    return(ChineseNames) 
  } else {
    unlist( lapply(as.character( ChineseNames ), py1) )
  }
}

py3 <- function (ChineseName) 
  # Translate Chinese names into English based on the pinyin package
  # The capitalize function is based the capitalize function in Hmsic package
  # ""武汉"" --> "Wuhan"
{

    string <- as.character( py(ChineseName, sep = "", dic = mypy) )
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
    return(string)

}
entireCountry <- "全国"
options(scipen=999) # turn off scientific notation like 1e+06
#daysForcasted = 10
options(warn=-1) #suppress warnings
# Data see: https://mp.weixin.qq.com/s/lrQWGKj-mReWrxfi_4Sw9A
library(nCov2019)
library(dplyr)
library(tidyr) # for gather function 
if(isEnglish) { 
  try( y <- get_nCov2019(lang="en"), silent = TRUE)  # load real time data from Tencent
  x <- load_nCov2019(lang="en", source = "dxy") #load historical data
  xgithub <- load_nCov2019(lang="en", source = "github") #load historical data 
} else {
  try( y <- get_nCov2019(lang="zh"), silent = TRUE)  # load real time data from Tencent
  x <- load_nCov2019(lang="zh", source = "dxy") #load historical data
  xgithub <- load_nCov2019(lang="zh", source = "github") #load historical data
  }
x$data <- x$data %>% 
  filter( time > as.Date("2020-1-10")) %>%
  filter( !(cum_confirm == 0 & cum_dead == 0 & cum_heal == 0) )

#x$global <- x$global %>% 
#  filter( time > as.Date("2020-1-10")) 

# correct an erorr in data "四川 " "四川"
x$data$province <- gsub(" ", "", x$data$province)
#x$data$province <- gsub("省|市", "", x$data$province)
x$data$city <- gsub(" ", "", x$data$city)

#Get a list of sorted provinces
provinceNames <- x$data %>% 
  arrange(province, desc(cum_confirm) ) %>%
  group_by(province) %>%
  filter(row_number() ==1) %>%
  arrange(desc(cum_confirm)) %>% 
  pull(province)

#provinceNames <- c(entireCountry, provinceNames)

# Get a list of cities sorted by cases
cityNames <- x$data %>% 
  filter(province != city) %>%
  arrange(city, desc(cum_confirm) ) %>%
  group_by(city) %>%
  filter(row_number() ==1) %>%
  arrange(desc(cum_confirm)) %>% 
  pull(city)

#Beijing, ...
specialProvinces <- c("北京", "上海", "重庆", "天津", "西藏")

# add province names to end of city names, as Beijing, Shanghai
cityNames = c(cityNames, provinceNames)

cityNamesList <- setNames(cityNames, cityNames)
if(isEnglish) 
  cityNamesList <- setNames(cityNames, py2( cityNames) )

#Today's totals
todayTotal <- do.call(rbind, Map(data.frame, total=y$chinaTotal,add=y$chinaAdd))
colnames(todayTotal) <- c("总数","增加")
rownames(todayTotal) <- c("确诊","疑似","死亡","痊愈","New Confirmed","NewSever")

# Use data from Tencent for historical China data
#ChinaHistory <- summary(y) %>%
#  mutate(time = as.Date( gsub("\\.","-",paste0("2020-",date) )) )

ChinaHistory <- x$data %>%
                mutate(cum_dead = as.integer(cum_dead)) %>%
               group_by(time) %>%
               summarise( confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                           dead = sum(cum_dead, na.rm = TRUE),
                           heal = sum(cum_heal,  na.rm = TRUE)) 

#ChinaHistory <- x$global %>%
#  filter(country == z('中国')) %>%
#  select( -country) %>%
#  rename( confirm = cum_confirm, heal = cum_heal, dead = cum_dead)

z <- function (ChineseName) {
  # translate chinese Names and menu items to English
  # it Uses a dictionary above
  if(!isEnglish) { 
    return(ChineseName) 
  } else {
    translated <- myDic2[ChineseName]
    if(is.na(translated)) 
      return( ChineseName ) else
        return(translated)
  }
}

z2 <- function (ChineseNames) 
  # Translate a vector of Chinese strings into English
  #  c("武汉", "上海") --> c("Wuhan", "Shanghai")
{
  if(!isEnglish) { 
    return(ChineseNames) 
  } else {
    unlist( lapply(as.character( ChineseNames ), z) )
  }
}

tem <- table(xgithub$global$country)
tem2 <- xgithub$global %>%
  group_by(country) %>%
  summarise(max = max(cum_confirm)) %>% 
  arrange(desc(max)) %>%
  filter(max > 30) %>%
  pull(country)

contriesPrediction <- xgithub$global %>%
  filter(country !='中国') %>%
  filter(country !='China') %>%
  filter(  country %in%  names(tem)[tem > 20]    ) %>% # only keep contries with 20 more data points.
  filter(  country %in%  tem2   ) %>%  # at least 20 cases
  filter (time > as.Date("2020-2-1")) %>%
  rename(dead = cum_dead, confirm = cum_confirm, heal = cum_heal)


# missing data imput using the mean of n neighboring data points on both sides
# if n = 1, then two neighbors, if n=2 then 2 neighbors on both sides
meanImput <- function (x, n = 2) { 
  ix <- is.na(x)
  x2 <- x
  for( ixx in which(ix)) {
    start <- ixx-n;
    if(start < 1) 
      start <- 1;
    end <- ixx + n;
    if(start > length(x)) 
      start <- length(x);  
    x2[ixx] <- mean( x[ start:end], na.rm = T  ) }
  return( x2 )
}

#Given a set of percentage increase or decrease, calculate final
# not 10% is represented as 10, not 0.1
increasesByPercentages <- function(x ) {
  increase = 1;
  for (i in 1:length(x))
    increase <- increase * (1 + x[i]/100)
  increase
}


finc <- function(x) {
  # format increases
  # convert 235 -> "+235"
  #         -235 -> "-235"
  if(x > 0) {
    return( paste0("+",x) )
  } else{
    return( as.character(x) )
  } 
}
#  Below come from #https://shiny.rstudio.com/gallery/unicode-characters.html
# for displaying Chinese characters
# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
      system('locate wqy-zenhei.ttc') != 0 &&
      !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    curl::curl_download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)


if (.Platform$OS.type == "windows") {
  if (!grepl("Chinese", Sys.getlocale())) {
    warning(
      "You probably want Chinese locale on Windows for this app",
      "to render correctly. See ",
      "https://github.com/rstudio/shiny/issues/1053#issuecomment-167011937"
    )
  }
}


myDic = matrix( c( 
  #---------------------Countries
  "中国", "China",
  "日本", "Japan",
  "新加坡","Singapore" ,
  "泰国" , "Thailand",
  "韩国" , "South Korea",
  "马来西亚", "Malaysia",
  "德国", "Germany",
  "越南", "Vietnam",
  "澳大利亚", "Australia",
  "美国", "USA",
  "法国", "France",
  "英国", "UK",
  "阿联酋", "UAE",
  "加拿大", "Canada",
  "印度" , "India",
  "菲律宾", "philippines",
  "意大利", "Italy",
  "西班牙", "Spain",
  "俄罗斯", "Russia",
  "比利时", "Belgium",
  "斯里兰卡", "Sri Lanka",
  "瑞典", "Sweden",
  "柬埔寨", "cambodia",
  "尼泊尔", "Nepal",
  "芬兰"  , "Finland",
  
  #---------------------Provinces
  "湖北", "Hubei",
  "广东", "Guangdong",
  "浙江", "Zhejiang",
  "河南", "Henan",
  "湖南", "Hunan",
  "安徽", "Anhui",
  "江西", "Jiangxi",
  "江苏", "Jiangsu",
  "山东", "Shandong",
  "重庆", "Chengqing",
  "四川", "Sicuan",
  "黑龙江", "Heilongjiang",
  "北京", "Beijing",
  "上海", "Shanghai",
  "福建", "Fujian",
  "河北" , "Hebei",
  "陕西" , "Shangxi",
  "广西", "Guangxi",
  "云南", "Yunnan",
  "海南", "Hainan",
  "山西", "Shanxi",
  "贵州", "Guizhou",
  "辽宁", "Liaoning",
  "天津", "Tianjin",
  "甘肃", "Gansu",
  "吉林", "Jilin",
  "内蒙古", "Inner Mongolia",
  "新疆", "Xinjiang",
  "宁夏", "Ningxia",
  "香港", "Hong Kong",
  "台湾", "Taiwan",
  "青海", "Qinghai",
  "澳门", "Macau",
  "西藏", "Tibet",
  
  #---------------------Menu items

  
  "疫情统计和预测", "Coronavirus COVID-19 outbreak statistics and forecast",
  
  "全国", "China",
  "地图", "Map",
  "省", "Provinces",
  "市", "Cities",
  "世界", "World",
  "预测", "Forecast",
  
  "确诊", "Confirmed",
  "死亡", "Death",
  "痊愈", "Discharged",
  
  "全国确诊:", "China total confirmed: ",
  ",   疑似:",  ",   suspected:",
  ",   死亡:",  ",   death:",
  ",   痊愈:", ",   discharaged:", 
  "一天之内数字会有多次更新。", "Updated serveral times a day. May not be final count for the day.",
  "01月", "Jan.",
  "02月", "Feb.",
  "03月", "March",
  "04月", "April",
  "05月", "May",
  "06月", "June",
  "07月", "July",
  "08月", "August",
  "09月", "Sept.",
  "10月", "Oct.",  
  "11月", "Nov.", 
  "12月", "Dec.", 
  "日", " ",
  "更新", "Updated",
  "北京时间", "Beijing time",
  "所有的图对数坐标 log10", "log10 scale for all plots",
  "(稍等几秒钟，地图下载)。", "Downloading map......",
  "选择预测天数", "Choose how many days to forecast",
  "简单的算法进行的预测,程序没有认真检查，仅供参考。用了R的forecast 软件包里的exponential smoothing 和forecast函数。",
       "We used a simple time series data forecasting model provided by the forecast package in R and the exponential smoothing method. We did not do rigrious testing of the models.",

  "先直接用全国的确诊总数的时间序列：", 
       "First we used the time series of the total confirmed cases in China to forecast:",
  
  "把全国的确诊总数先换算成了每天比前一天增加的百分比，
                 去除了前面10天不稳定的数据, 再预测：",
  "We transformed the data into daily increased percentage, and run the forecast:",
  
  "直接用全国的死亡累计数预测：", "Forecasting the total deathes in China directly:",
  
  "把全国的死亡累计数先换算成了每天比前一天增加的百分比，去除了前面10天不稳定的数据,再预测：",
     "Forecasting the daily percent increase:",
  
  "各市", "Cities",
  "确诊 (死亡)", "Confirmed (dead)",
  "腾迅", " from Tencent",
  "世界各国确诊 (死亡)", "Confirmed (dead) in countries outside China",
  "全国总数", "China total",
  "全国每日新增百分比", "% daily increases in China",
  "总数", "Total",
  "新增", " New cases",
  "预期", "Prediction:",
  "全国确诊", "Total confirmed cases in China",
  "天后全国确诊 ", " days later, total confirmed in China will be ",
  "预期全国确诊每天增加", "Predicted % daily increase/decrease  ",
  ", 区间[", ", 95% CI [",
  "死亡人数增加百分比(%)","% increase in death",
  "预期全国死亡累计每天增加", "Predicted % daily increase in death",
  "天后达到 ", " days later will be ",
  "全国死亡人数", "Total deathes in China",
  "天后全国死亡累计", " days later total deathes in China will be ",
  "全国各省", "Confirmed cases across provinces",
  "英语","中文", 
  "数据", "Data",
  "中国数据下载", "Download Data for China",
  "世界数据下载", "Download Data for the World",
  "湖北以外", " provinces without Hubei",
  "其他国家感染人数","Confirmed cases outside China",
  "武汉死亡率",  "Death rate in Wuhan: ",
  ", 其他城市: ", ", other cities: ",
  "武汉以外主要城市确诊数",  "Confirmed cases in cities excluding Wuhan",
  "死亡人数"  ,"Deaths",
  "各主要城市确诊数", "Confirmed cases in affected cities",
  "天后确诊 ", "days later confirmed cases in ",
  
  "last", "last"
),nrow=2)
# make a vector value is English, Name is chinese
myDic2 <- myDic[2,]
names(myDic2) <- myDic[1,]

z <- function (ChineseName) {
  # translate chinese Names and menu items to English
  # it Uses a dictionary above
  if(!isEnglish) { 
    return(ChineseName) 
  } else {
    translated <- myDic2[ChineseName]
    if(is.na(translated)) 
      return( ChineseName ) else
        return(translated)
  }
}

z2 <- function (ChineseNames) 
  # Translate a vector of Chinese strings into English
  #  c("武汉", "上海") --> c("Wuhan", "Shanghai")
{
  if(!isEnglish) { 
    return(ChineseNames) 
  } else {
    unlist( lapply(as.character( ChineseNames ), z) )
  }
}
provinceNamesList <- setNames(provinceNames, z2(provinceNames) )

