
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("cum_confirm", "cum_dead", 
    "cum_heal", "province", "city", "country", "name", "type", "count", "heal",
    "percentage", "cum_heal", "confirmed"))



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

#暂时没去看每个包使用了哪些函数，先全部import
#install.packages("pinyin")
#install.packages(c("sp","mapproj","maps","sf"))
#sf，maps已有
#install.packages(c("forcats","ggrepel","forecast","plotly","shinyBS","lubridate"))


##' @title Shiny app
##' @description a shiny app
##' @param lang Your language
##' @param remote If TRUE, open the online version
##' @import shiny
##' @import shinyBS
##' @import pinyin
##' @import sp
##' @import mapproj
##' @import sf
##' @import forcats
##' @import ggrepel
##' @import forecast
##' @import lubridate
##' @import plotly
##' @importFrom graphics axis
##' @importFrom graphics par
##' @importFrom stats setNames
##' @importFrom stats ts
##' @importFrom dplyr row_number
##' @importFrom dplyr pull
##' @importFrom tidyr gather
##' @importFrom dplyr recode_factor
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 geom_point
##' @importFrom ggplot2 geom_line
##' @importFrom ggplot2 geom_col
##' @importFrom ggplot2 geom_col
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 theme_gray
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 ggtitle
##' @importFrom ggplot2 coord_flip
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom ggplot2 scale_y_log10
##' @importFrom ggplot2 expand_limits
##' @importFrom ggplot2 geom_smooth
##' @importFrom ggplot2 scale_x_log10
##' @importFrom dplyr desc
##' @importFrom curl curl_download
##' @importFrom chinamap get_map_china
##' @importFrom ggrepel geom_text_repel
##' @importFrom stats t.test
##' @export
open_dashboard <- function(lang="auto", remote=FALSE) {
    # run shinyApp
    #turn off scientific notation like 1e+06
    op_scipen <- as.numeric(options("scipen"))
    old_opt_scipen <- options(scipen = op_scipen)
    options(scipen=999)
    on.exit(options(old_opt_scipen), add = TRUE)

    #suppress warnings
    op_warn <- as.numeric(options("warn"))
    old_opt_warn <- options(warn = op_warn)
    options(warn=-1)
    on.exit(options(old_opt_warn), add = TRUE)
    
    # Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
    op_shiny <- as.logical(unlist(options("shiny.usecairo")))
    old_opt_shiny <- options(shiny.usecairo = op_warn)
    options(shiny.usecairo = FALSE)
    on.exit(options(old_opt_shiny), add = TRUE)
    
    lang <- which_lang(lang) #zh or en
    if (remote) {
        if (lang == 'zh') {
            utils::browseURL('http://www.bcloud.org/v/')
        } else {
            utils::browseURL('http://www.bcloud.org/e/')
        }
    } else {
        if (lang != 'zh')
            isEnglish <- TRUE else isEnglish <- FALSE
        z <- function (ChineseName) {
            # translate chinese Names and menu items to English
            # it Uses a dictionary above
            font_home <- function(path = '') file.path('~', '.fonts', path)
              if (Sys.info()[['sysname']] == 'Linux' &&
                    system('locate wqy-zenhei.ttc') != 0 &&
                    !file.exists(font_home('wqy-zenhei.ttc'))) {
                if (!file.exists('wqy-zenhei.ttc'))
                  #curl::curl_download(
                  curl_download(
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
    
        mypy <- pydic(method = "toneless",dic = 'pinyin2')    
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
        provinceNamesList <- setNames(provinceNames, z2(provinceNames) )
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
        tem <- table(xgithub$global$country)
        tem2 <- xgithub$global %>%
            group_by(country) %>%
            summarise(max = max(cum_confirm)) %>%
            filter(max > 20) %>%
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
        
        
        
        if(isEnglish)
            myURL = "http://www.bcloud.org/v/"  else
                myURL = "http://www.bcloud.org/e/" 
        plotWidth <- 800
        entireCountry <- "全国"
        
        
        
        #UI
        ui <- fluidPage(
            titlePanel(z("疫情统计和预测")),
            tabsetPanel(
            tabPanel(z("全国")
            ,h4( paste0( 
                         z("全国确诊:"), y$chinaTotal$confirm, " (", finc(y$chinaAdd$confirm), ", ", finc( round(y$chinaAdd$confirm/(y$chinaTotal$confirm - y$chinaAdd$confirm)*100,1)  ), "%)",
                         z(",   疑似:"), y$chinaTotal$suspect, " (", finc(y$chinaAdd$suspect), ", ", finc( round(y$chinaAdd$suspect/(y$chinaTotal$suspect - y$chinaAdd$suspect)*100,1)  ), "%)",
                         z(",   死亡:"), y$chinaTotal$dead,    " (", finc(y$chinaAdd$dead),    ", ", finc( round(y$chinaAdd$dead/(y$chinaTotal$dead - y$chinaAdd$dead)*100,1)  ), "%)",
                        z(",   痊愈:"),  y$chinaTotal$heal,    " (", finc(y$chinaAdd$heal),    ", ", finc( round(y$chinaAdd$heal/(y$chinaTotal$heal - y$chinaAdd$heal)*100,1)  ), "%)"
                         )
                 )
            ,h5(    z("更新"), "     ", z( paste0( gsub("-.*","", gsub(" .*|2020-","",y$lastUpdateTime)), "月")),
                    gsub(".*-","", gsub(" .*|2020-","",y$lastUpdateTime)), z("日"),"  ", z("北京时间"), "  ", gsub(".* ","", y$lastUpdateTime),". ",
                    z("一天之内数字会有多次更新。") )
            
            ,fluidRow( 
              column(6, checkboxInput("logScale", z("所有的图对数坐标 log10"), value = FALSE) )
#              ,column(2, downloadButton('dataDownload', z('下载')	) )
              ,column(6, align="right",a(z("英语"),  href=myURL) )
              )
            
            #,plotlyOutput("historicalChinaDataPlotly")
            #这个动图还没搞定
            #,img(src=system.file("ChinaMapAnimated2.gif", package="nCov2019"), align = "center",width="600", height="450")
            #,img(src="https://github.com/gexijin/wuhan/tree/master/shinyapps/e/www/ChinaMapAnimated2.gif", align = "center",width="600", height="450")
            ,br(),br()
            ,plotOutput("realTimeProvinceConfirmed")
            ,br()
            ,plotlyOutput("historicalChinaData")
             ,br()
            ,plotlyOutput("historicalChinaDataAdd")
            ,br()
            ,plotOutput("confirmedByProvincesHistorical")    
            , br()
            ,plotlyOutput("deathRatesCities")
            
            
            
            
            ,br()
            
            ) #tab1 --------------------------------------------------
            
            ,tabPanel(z("地图")
                      ,h4(paste0( z(paste0(gsub("-.*","", gsub(" .*|2020-","",y$lastUpdateTime)), "月")),
                              gsub(".*-","", gsub(" .*|2020-","",y$lastUpdateTime)), z("日")), z("(稍等几秒钟，地图下载)。"))
                      ,plotOutput("ChinaMap")
            
                      
            )
            
            ,tabPanel(z("省")
                      ,selectInput("selectProvince0", NULL, choices = provinceNamesList)
            #,tableOutput("todayTotalTable")
            ,plotOutput("realTimeCityConfirmed") 
            ,br()
            ,plotlyOutput("provienceHistorical")      
            ,plotlyOutput("provienceHistoricalAdd")      
            ,plotOutput("cities_in_proviences")    
            ,br()
            #,plotlyOutput("cities_in_proviences_selected_plotly")
            
            ,plotOutput("provinceMap") 
            ,br()
            )
            
            ,tabPanel(z("市")
                      ,fluidRow( 
                        column(3, selectInput("selectProvince", NULL, choices = provinceNamesList)  ),
                        column(3, selectInput("selectCity", NULL, choices = cityNames))
                      )
                      ,plotlyOutput("cities_in_proviences_selected")        
                      ,plotlyOutput("cities_in_proviences_selectedAdd")  
                      )
            
            ,tabPanel(z("世界")
                      ,plotOutput("realTimeCityConfirmedWorld")
                      ,br()
                      ,plotlyOutput("historicalWorld")
                      ,br()
                      ,plotOutput("worldMap")            
                      
                      )#tab2 --------------------------------------------------
            
            ,tabPanel(z("预测") 
                      ,sliderInput("daysForcasted", z("选择预测天数"),
                                     min = 1, max = 7,
                                     value = 10)
                      ,h5(z("简单的算法进行的预测,程序没有认真检查，仅供参考。用了R的forecast 软件包里的exponential smoothing 和forecast函数。") )
                      ,h5(z("先直接用全国的确诊总数的时间序列："))
                     ,plotOutput("forecastConfirmedRaw")
                     ,br()   
                     ,br() 
                     ,h5(z("把全国的确诊总数先换算成了每天比前一天增加的百分比，
                         去除了前面10天不稳定的数据, 再预测："))
                     ,plotOutput("forecastConfirmedChange")
                     ,br()
                     ,br()
                     ,h5(z("直接用全国的死亡累计数预测："))
                     ,plotOutput("forecastDeadRaw")
                     ,br()
                     
                     ,br()
                     ,h5(z("把全国的死亡累计数先换算成了每天比前一天增加的百分比，去除了前面10天不稳定的数据,再预测：") )
                     ,plotOutput("forecastDeadChange")
                     ,br(),br()
                     ,selectInput("selectCountry", NULL, choices = unique(contriesPrediction$country))
                     ,plotOutput("forecastConfirmedChangeWorld")
                     
            
            ) #tab2 --------------------------------------------------
            ,tabPanel(z("数据") 
                      ,br()
                      , downloadButton('dataDownload', z('中国数据下载')	)
                      ,br(),br()
                      , downloadButton('dataDownloadWorld', z('世界数据下载')	)             
                      ,br()
                      
            ) #tab2 --------------------------------------------------
            ,tabPanel("?"
                      ,h4("不保证数据和分析的可靠性，仅供参考。", style = "color:red")
            ,h5("该网站是我工作之余仓促码出来的, 难免有错误。见",
                a("源代码。 ", href="https://github.com/gexijin/wuhan"),
                "主要目的是帮助朋友们了解疫情。纯粹个人行为。",
                "bcloud.org 是以前注册的一个域名，随手拿来用了，不属于任何组织。"
                )
            ,h5("之所以能很快写出来，最主要是因为南方医科大学的",
                 a("余光创教授",  href="http://portal.smu.edu.cn/jcyxy/info/1084/2203.htm"),
                "(微信公众号biobabble）写了一个功能强大的下载实时数据的软件包：",
                a("nCov2019。", href="https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247488621&idx=1&sn=727f8bdec2801ddc0315b9fedaa40acc&scene=21"),
                "实时数据来自腾讯， 每天更新。历史数据从【新一线城市研究所×8点健闻】。好像不是每天都更新。")
            ,h5("有意见或建议可以给我发"
                ,a("邮件",href="mailto:xijin.ge@sdstate.edu?Subject=疫情网站" ),"。 ",
                "我做生物信息学方面的研究，用计算的方法探索生命的奥秘 (",
                a("研究室网页", href="http://ge-lab.org/"), ")。"  )
            ,h5("武汉加油！ 中国加油！")
            
            ,h4("Accuracy not guaranteed. Not official data.", style = "color:red")
            ,h5("This website tracks the cases of the 2019-nCoV (SARS-Cov-2) coronavirus originated from Wuhan, China. Developed on Feb 5, 2020 by",a("Ge Xijin", href="https://twitter.com/StevenXGe"),
                "based on the R package", a("nCov2019",href="https://github.com/GuangchuangYu/nCov2019"), 
                "by", a("Dr. Guangchuang Yu.", href="https://twitter.com/guangchuangyu"))
            ,h5("Feedbacks or suggestions please"
                ,a("email me",href="mailto:xijin.ge@sdstate.edu?Subject=Coronavirus website" ),".",
                "I am bioinformatics researcher (",
                a("lab", href="http://ge-lab.org/"), ")."  )
            ,h5("I am not a epidemiologists, or statistician. Just got too much time on my hand")
            ,h5("All rights reserved.")
            
            ,h5("2/5/20  Version 0")  
            ,h5("2/8/20  Version 0.1")
            ,h5("2/9/20 Version 0.2 ")
            ,h5("2/12/20 V 0.3 English version")
            ,h5("2/23/20 v. 0.4 Interactive plots.")
            )
            )
            ,tags$head(includeScript(system.file("ga.js", package="nCov2019"))) # tracking usage with Google analytics      
        )           
    

        server <- function(input, output, session) {
            observe({  
                cityNamesProvince <- unique( x[input$selectProvince,]$city )
                ix <- match(cityNames, cityNamesProvince)
                
                updateSelectInput(session, "selectCity", NULL, choices = cityNamesList[!is.na(ix)] ) 
                if( input$selectProvince == entireCountry ) 
                    updateSelectInput(session, "selectCity", NULL, choices = NULL )    
                
                })
    
            output$todayTotalTable <- renderTable(todayTotal,rownames = TRUE, colnames = TRUE, bordered = TRUE)
            
            #各个省 确诊 历史数  -------------------------------------------    
            output$confirmedByProvincesHistorical <- renderPlot({
            
                    d2 <- xgithub$province %>%
                      filter( province != "湖北") %>%
                      filter( province != "Hubei") 
                    
                    if(isEnglish) d2$province <- py2( d2$province )  # translate into Pinyin
                    p <- ggplot(d2,
                                aes(time, as.numeric(cum_confirm), group=province, color=province)) +
                        geom_point() + geom_line() +
                        geom_text_repel(aes(label=province),  family="SimSun",data=d2[d2$time == time(x), ], hjust=1) +
                        theme_gray(base_size = 14) + theme(legend.position='none') +
                        xlab(NULL) + ylab(NULL) + 
                        ggtitle(paste( z(entireCountry),  z("湖北以外"),  xgithub$time ) )         
            
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                p
                
            }, width = plotWidth - 100 )
            
            #省内各城市 确诊 历史数  -------------------------------------------    
            output$cities_in_proviences <- renderPlot({
            
                d <- x[input$selectProvince0, ]
                if(isEnglish) d$city <- py2( d$city )  # translate into Pinyin
                p <- ggplot(d,
                       aes(time, as.numeric(cum_confirm), group=city, color=city)) +
                    geom_point() + geom_line() +
                    geom_text_repel(aes(label=city), family="SimSun",data=d[d$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14) + theme(legend.position='none') +
                    xlab(NULL) + ylab(NULL) + 
                    ggtitle(paste(z(input$selectProvince0), z("各市"),  x$time) )
            
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                p
            
            }, width = plotWidth - 100 )
            
            #全国 当天 确诊 数  -------------------------------------------
            output$realTimeProvinceConfirmed <- renderPlot({
            
                d = y[]; d <- d[1:20, ]
                d$confirm=as.numeric(d$confirm)
                if(isEnglish) d$name <- py2( d$name )  # translate into Pinyin
                d$name = fct_reorder(d$name, d$confirm)        
                
                # This is used to create spaces so the numbers on top of the bar shows up.
                maxN <- max(d$confirm) *1.5
                if(input$logScale) 
                    maxN <- max(d$confirm) *10
                
                
                p <- ggplot(d, aes(name, confirm)) + 
                    geom_col(fill='steelblue') + coord_flip() +
                    geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
                    theme_gray(base_size=14) + 
                    scale_y_continuous(expand=c(0,10)) +
                    xlab(NULL) + ylab(NULL) +
                    theme(text = element_text(size=17, family="SimSun"),
                          axis.text.x = element_text(angle=0, hjust=1))  + 
                    #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
                    ggtitle(paste( z("确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
                    expand_limits(y = maxN)+ 
                  theme(plot.title = element_text(size = 15))
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                p
                
            }, width = plotWidth - 100) 
            
            
            #省内各个城市当天确诊数  -------------------------------------------
            output$realTimeCityConfirmed <- renderPlot({
                d = y[input$selectProvince0,] 
                d$confirm=as.numeric(d$confirm)
                if(isEnglish) d$name <- py2( d$name )  # translate into Pinyin
                d$name = fct_reorder(d$name, d$confirm)
                
                # This is used to create spaces so the numbers on top of the bar shows up.
                maxN <- max(d$confirm) *1.5
                if(input$logScale) 
                    maxN <- max(d$confirm) *10
                
                p <- ggplot(d, aes(name, confirm)) + 
                    geom_col(fill='steelblue') + coord_flip() +
                    geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
                    theme_gray(base_size=14) + 
                    scale_y_continuous(expand=c(0,10)) +
                    xlab(NULL) + ylab(NULL) +
                    theme(text = element_text(size=17, family="SimSun"),
                          axis.text.x = element_text(angle=0, hjust=1))  + 
                    #ggtitle(paste("confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
                    ggtitle(paste( z(input$selectProvince0), z("确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
                    expand_limits(y = maxN)+ 
                  theme(plot.title = element_text(size = 15))
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                p
                
            }, width = plotWidth - 100 ) 
            
            #各个城市死亡率  -------------------------------------------
            output$deathRatesCities <- renderPlotly({
              d = x$data %>% 
                arrange( desc(time))  %>%
                #arrange( desc(c(time, city)))  %>%
                filter(!duplicated(city)) %>% 
                filter(province != city ) %>%
                filter(cum_dead > 1)  %>%
                filter(cum_confirm > 50) %>%
                mutate(rate = 100*cum_dead/cum_confirm) 
                
            
              deathRate = paste0(z("武汉死亡率"), round(d$cum_dead[1]/d$cum_confirm[1]*100 ,2), "%",
                                 z(", 其他城市: "), round( mean(d$rate) ,2), "%, [", 
                                 round(t.test(d$rate[-1])$conf.int[1],2), "%-",
                                 round(t.test(d$rate[-1])$conf.int[2],2),"%]"
                                 )      
              
              if(isEnglish) d$province <- py2( d$province )  # translate into Pinyin      
              #if(isEnglish) 
                d$city <- py3( d$city )       
              p <- ggplot(d[-1, ], aes(cum_confirm, cum_dead, color = province, text=city)) +
                xlab(z("各主要城市确诊数")) 
            
              
              if(input$logScale) {
                p <- ggplot(d[, ], aes(cum_confirm, cum_dead, color = province, text=city)) +
                scale_y_log10() +
                scale_x_log10() +
                xlab(z("各主要城市确诊数")) 
            
              }
              
              p <- p +
                geom_point(size = 3) + 
                geom_smooth(method = "lm", 
                            inherit.aes = FALSE, 
                            aes(cum_confirm, cum_dead), 
                            se = FALSE, color = "darkgrey",
                            linetype = "dashed") +
                ylab(z("死亡人数")) +
                ggtitle(deathRate) +
                theme(plot.title = element_text(size = 10))
              
              ggplotly(p, tooltip = c("y", "x","text")) %>% 
                layout( width = plotWidth)
              
            } ) 
            
            #世界各国分布图，现在的数据 -------------------------------------------
            output$realTimeCityConfirmedWorld <- renderPlot({
                d <- y['global',] %>%
                  filter(!is.na(name)) %>%
                  mutate( confirm =as.numeric(confirm) ) %>%
                  mutate (name = z2( name ) ) %>%
                  mutate( name = fct_reorder(name, confirm))
                
                d <- d[-1, ] #remove the first row
                d <- d[1:20, ]
                
                
                # This is used to create spaces so the numbers on top of the bar shows up.
                maxN <- max(d$confirm) *1.5
                if(input$logScale) 
                    maxN <- max(d$confirm) *20
                
                p <- ggplot(d, aes(name, confirm)) + 
                    geom_col(fill='steelblue') + coord_flip() +
                    geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
                    theme_gray(base_size=14) + 
                    scale_y_continuous(expand=c(0,10)) +
                    xlab(NULL) + ylab(NULL) +
                    theme(text = element_text(size=17, family="SimSun"),
                          axis.text.x = element_text(angle=0, hjust=1))  + 
                    #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
                    ggtitle(paste(z("世界各国确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
                    expand_limits(y = maxN) + 
                    theme(plot.title = element_text(size = 15))
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                p
                
            }, width = plotWidth - 100 ) 
            
            
            
            #世界细节 历史图 -------------------------------------------
            output$historicalWorld <- renderPlotly({
              
              tem <- table(xgithub$global$country)
              
              tem2 <- xgithub$global %>%
                group_by(country) %>%
                summarise(max = max(cum_confirm)) %>%
                filter(max > 20) %>%
                pull(country)
              
              d <- xgithub$global %>%
                filter(country !=z('中国')) %>%
                filter(  country %in%  names(tem)[tem > 10]    ) %>% # only keep contries with 20 more data points.
                filter(  country %in%  tem2   ) %>%  # at least 20 cases
                filter (time > as.Date("2020-2-1"))
              
              p <- ggplot(d,
                     aes(time, cum_confirm, group=country, color=country)) +
                geom_point() + geom_line() +
                geom_text_repel(aes(label=country), data=d[d$time == time(x), ], hjust=1) +
                theme_gray(base_size = 12) + #theme(legend.position='none') +
                xlab(NULL) + ylab(NULL) + #xlim(as.Date(c("2020-01-15", "2020-03-01"))) +
                ggtitle (z("其他国家感染人数")) +
                theme(plot.title = element_text(size = 12))
              
              if(input$logScale) 
                p <- p + scale_y_log10() 
              
              ggplotly(p, tooltip = c("y", "x","country")) %>% 
                layout( width = plotWidth)
              
            })
            
            #全国细节 历史图 -------------------------------------------
            output$historicalChinaData <- renderPlotly({
              
                dl <- ChinaHistory %>%
                    gather( type, count, c(confirm, heal, dead)) %>%
                    mutate( type = recode_factor(type,
                                                 confirm = z("确诊"),
                                                 dead = z("死亡"),
                                                 heal = z("痊愈")))
            
                p <- ggplot(dl,
                            aes(time, count, group=type, color=type)) +
                    geom_point() + geom_line() +
                    geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14) + #theme(legend.position='none') +
                    xlab(NULL) + ylab(NULL)  +
                    theme(legend.title = element_blank()) +
                    theme(plot.title = element_text(size = 11))
            
                    p <- p + ggtitle(paste( z("全国总数"),  x$time) ) 
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                ggplotly(p, tooltip = c("y", "x")) %>% 
                  layout( width = plotWidth)
                
            })
            
            #全国细节 历史图 增加-------------------------------------------
            output$historicalChinaDataAdd <- renderPlotly({
                pc <- ChinaHistory
                pc[2:nrow(pc), 2:4] <- (pc[2:nrow(pc), 2:4] / pc[1:(nrow(pc)-1), 2:4] -1 )*100
                pc <- pc[-1, ] %>%
                  filter( heal <100) %>%
                  filter(time > "2020-1-28")
                
                
                dl <- pc %>%
                    gather( type, percentage, c(confirm, heal, dead)) %>%
                    mutate( type = recode_factor(type,
                                                 confirm = z("确诊"),
                                                 dead = z("死亡"),
                                                 heal = z("痊愈"))) %>%
                    filter( type !=  z("痊愈") )
                p <- ggplot(dl, aes(time, percentage, group=type, color=type)) +
                    geom_point() + geom_line() +
                    geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14) + #theme(legend.position='none') +
                    ylab(NULL) + xlab(NULL) +
                    theme(legend.title = element_blank()) +
                    theme(plot.title = element_text(size = 11))
                
                p <- p + ggtitle(paste(z("全国每日新增百分比"),  x$time) ) 
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                ggplotly(p, tooltip = c("y", "x")) %>% 
                  layout( width = plotWidth)
                
            })
            
            #全国细节 历史图 增加-------------------------------------------
            output$historicalChinaDataAddRaw <- renderPlotly({
              
              d2 <- ChinaHistory 
              
              d3 <- d2[-1, ] %>%
                mutate(confirm = diff(d2$confirm)) %>%     
                mutate(dead = diff(d2$dead)) %>%
                mutate(heal = diff(d2$heal))
              
              # add a row with zeros but with date; so that the two figures align
              d3 <- rbind(d2[1, ], d3)
              d3[1, 2:4] <- 0;
              
              dl <- d3 %>%
                gather( type, count, c(confirm, heal, dead)) %>%
                mutate( type = recode_factor(type,
                                             confirm = z("确诊"),
                                             dead = z("死亡"),
                                             heal = z("痊愈")))
              p <- ggplot(dl,
                          aes(time, count, group=type, color=type)) +
                geom_point() + geom_line() +
                geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                theme_gray(base_size = 14) + #theme(legend.position='none') +
                xlab(NULL) + ylab(NULL) +
                theme(legend.title = element_blank()) +
                theme(plot.title = element_text(size = 13))
              
              p <- p + ggtitle(paste(z("全国每日新增"),  x$time) ) 
              
              if(input$logScale) 
                p <- p + scale_y_log10() 
              ggplotly(p, tooltip = c("y", "x")) %>% 
                layout( width = plotWidth)
              
            })    
            
            #省 历史图 新增-------------------------------------------
            output$provienceHistoricalAdd <- renderPlotly({
                d2 <- x[input$selectProvince0, ]  %>% 
                    mutate(cum_dead = as.integer(cum_dead)) %>%
                    select( city, time, cum_confirm, cum_dead, cum_heal) %>%
                    group_by(time) %>%
                    summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                               cum_dead = sum(cum_dead, na.rm = TRUE),
                               cum_heal = sum(cum_heal,  na.rm = TRUE)) %>%
                    arrange( order(time)) %>%
                    mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
                    mutate( cum_dead = meanImput(cum_dead, 2)) %>%
                    mutate( cum_heal = meanImput(cum_heal, 2)) 
                
            
                d3 <- d2[-1, ] %>%
                    mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
                    mutate(cum_dead = diff(d2$cum_dead)) %>%
                    mutate(cum_heal = diff(d2$cum_heal)) 
                
                # add a row with zeros but with date; so that the two figures align
                d3 <- rbind(d2[1, ], d3)
                d3[1, 2:4] <- 0;
                
                dl <- d3 %>%
                    gather( type, count, cum_confirm:cum_heal) %>%
                    mutate( type = recode_factor(type,
                                                 cum_confirm = z("确诊"),
                                                 cum_dead = z("死亡"),
                                                 cum_heal = z("痊愈")))
                
                p <- ggplot(dl,
                            aes(time, count, group=type, color=type)) +
                    geom_point() + geom_line() +
                    geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14) + #theme(legend.position='none') +
                    xlab(NULL) + ylab(NULL)  +
                    theme(legend.title = element_blank()) +
                  theme(plot.title = element_text(size = 13))
                
                p <- p + ggtitle(paste(z(input$selectProvince0), z("新增"),  x$time) ) 
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                ggplotly(p, tooltip = c("y", "x")) %>% 
                  layout( width = plotWidth)
                
            })
            
            #省 历史图  -------------------------------------------
            output$provienceHistorical <- renderPlotly({
                d2 <- x[input$selectProvince0, ]  %>% 
                    mutate(cum_dead = as.integer(cum_dead)) %>%
                    select( city, time, cum_confirm, cum_dead, cum_heal) %>%
                    group_by(time) %>%
                    summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                               cum_dead = sum(cum_dead, na.rm = TRUE),
                               cum_heal = sum(cum_heal,  na.rm = TRUE)) %>%
                    arrange( order(time)) %>%
                    mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
                    mutate( cum_dead = meanImput(cum_dead, 2)) %>%
                    mutate( cum_heal = meanImput(cum_heal, 2)) 
                
                dl <- d2 %>%
                    gather( type, count, cum_confirm:cum_heal) %>%
                    mutate( type = recode_factor(type,
                                                 cum_confirm = z("确诊"),
                                                 cum_dead = z("死亡"),
                                                 cum_heal = z("痊愈")))
                
                p <- ggplot(dl,
                            aes(time, as.numeric(count), group=type, color=type)) +
                    geom_point() + geom_line() +
                    geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14) + 
                    #theme(legend.position='none') +
                    xlab(NULL) + ylab(NULL)  +
                    theme(legend.title = element_blank()) +
                  theme(plot.title = element_text(size = 13))
                
                p <- p + ggtitle(paste(z(input$selectProvince0), z("总数"),  x$time) ) 
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                
                ggplotly(p, tooltip = c("y", "x")) %>% 
                  layout( width = plotWidth)
                
            })
                    
            #城市细节 历史图 -------------------------------------------
            output$cities_in_proviences_selected <- renderPlotly({
            
                    d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
                        mutate(cum_dead = as.integer(cum_dead)) %>%
                        select( time, cum_confirm, cum_dead, cum_heal) %>%
                        mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
                        mutate( cum_dead = meanImput(cum_dead, 2)) %>%
                        mutate( cum_heal = meanImput(cum_heal, 2)) 
                    
                    dl <- d2 %>%
                        gather( type, count, cum_confirm:cum_heal) %>%
                        mutate( type = recode_factor(type,
                                                     cum_confirm = z("确诊"),
                                                     cum_dead = z("死亡"),
                                                     cum_heal = z("痊愈")))
                    
                    p <- ggplot(dl,
                                aes(time, count, group=type, color=type)) +
                                geom_point() + geom_line() +
                                #geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                                theme_gray(base_size = 14) + #theme(legend.position='none') +
                                xlab(NULL) + ylab(NULL) +
                                theme(legend.title = element_blank()) +
                                theme(plot.title = element_text(size = 13))
                    p <- p + ggtitle(paste( py1(input$selectCity), z("总数"), z( "更新"), x$time) )                    
            
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                    
                ggplotly(p, tooltip = c("y", "x")) %>% 
                  layout( width = plotWidth)
                
            })
            
            
            #城市细节 历史图 新增-------------------------------------------
            output$cities_in_proviences_selectedAdd <- renderPlotly({
                
                d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
                    mutate(cum_dead = as.integer(cum_dead)) %>%
                    select( time, cum_confirm, cum_dead, cum_heal) %>%
                    mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
                    mutate( cum_dead = meanImput(cum_dead, 2)) %>%
                    mutate( cum_heal = meanImput(cum_heal, 2)) %>%
                    arrange(order(time) )
                
                d3 <- d2[-1, ] %>%
                    mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
                    mutate(cum_dead = diff(d2$cum_dead)) %>%
                    mutate(cum_heal = diff(d2$cum_heal))     
                
                # add a row with zeros but with date; so that the two figures align
                d3 <- rbind(d2[1, ], d3)
                d3[1, 2:4] <- 0;
                
                dl <- d3 %>%
                    gather( type, count, cum_confirm:cum_heal) %>%
                    mutate( type = recode_factor(type,
                                                 cum_confirm = z("确诊"),
                                                 cum_dead = z("死亡"),
                                                 cum_heal = z("痊愈")))
              
                p <- ggplot(dl,
                            aes(time, as.numeric(count), group=type, color=type)) +
                    geom_point() + geom_line() +
                    geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14) + #theme(legend.position='none') +
                    xlab(NULL) + ylab(NULL) +
                    theme(legend.title = element_blank())+
                    theme(plot.title = element_text(size = 13)) 
            
                    p <- p + ggtitle(paste( py1(input$selectCity), z("新增"),  x$time ) )                   
            
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                ggplotly(p, tooltip = c("y", "x")) %>% 
                  layout( width = plotWidth)
                
            })
            
            
            #城市细节 历史图 Plotly-------------------------------------------
            output$cities_in_proviences_selected_plotly <- renderPlotly({
                    d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
                        mutate(dead = as.integer(dead)) %>%
                        select( time, confirmed, dead, heal) 
            
                
                dl <- d2 %>%
                    gather( type, count, confirmed:heal) %>%
                    mutate( type = recode_factor(type,
                                                 confirmed = z("确诊"),
                                                 dead = z("死亡"),
                                                 heal = z("痊愈")))
               
                p <- ggplot(dl,
                            aes(time, count, group=type, color=type)) +
                    geom_point() + geom_line() +
                    #geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                    theme_gray(base_size = 14)  + theme(legend.title = element_blank() ) +
                    xlab(NULL) + ylab(NULL) 
            
                    p <- p + ggtitle(paste(input$selectCity,  x$time ) )                   
            
                
                if(input$logScale) 
                    p <- p + scale_y_log10() 
                ggplotly(p, tooltip = c("y", "x"))%>% 
                  layout( width = plotWidth)
                
            })
            
            
              
            #世界 各国确诊人数预测, 预测-------------------------------------------    
            
              output$forecastConfirmedChangeWorld <- renderPlot ({
                d2 <- contriesPrediction %>%
                  arrange(time) %>%
                  filter( country == input$selectCountry)
                nRep = sum( d2$confirm == d2$confirm[2]) 
                if(nRep > 3) 
                  d2 <- d2[-(1:(nRep-3)),]
                
                par(mar = c(4, 3, 0, 2))
                # missing data with average of neighbors
                d2$confirm<- meanImput(d2$confirm, 2)
                
                confirm <- ts(d2$confirm, # percent change
                              start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
                forecasted <- forecast(ets(confirm), input$daysForcasted)
                plot(forecasted, xaxt="n", main="", 
                     ylab = z("全国确诊"),
                     xlab = paste0(z("预期"), input$daysForcasted, z("天后确诊"),input$selectCountry, " ", round(forecasted$mean[input$daysForcasted],0), z(", 区间["),
                                   round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
                )
                a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
                axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
              }, width = plotWidth - 100 ) 
            
            
            #全国确诊人数预测, 百分比预测-------------------------------------------    
            output$forecastConfirmedChange <- renderPlot ({
                d2 <- ChinaHistory
                par(mar = c(4, 3, 0, 2)) 
                # missing data with average of neighbors
                d2$confirm<- meanImput(d2$confirm, 2)
                d2 <- d2[-(1:20), ] # remove the first 10 days as % change is huge
                
                confirm <- ts(diff(d2$confirm)/(10 + d2$confirm[1:(nrow(d2)-1)])*100, # percent change
                                start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )
                
                forecasted <- forecast(ets(confirm), input$daysForcasted)
                
                predictedNconfirm = d2$confirm[nrow(d2)]* increasesByPercentages(forecasted$mean)       
                plot(forecasted, xaxt="n", main="", 
                     ylab = z("全国确诊增加百分比(%)"),
                     xlab = paste0(z("预期全国确诊每天增加"), round( mean( forecasted$mean ), 1 ),
                                  "%，", input$daysForcasted, z("天后达到 "), round(predictedNconfirm,0) )            
                     )
                a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
                axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
            }, width = plotWidth - 100 )
            
            #全国确诊人数预测, 直接预测-------------------------------------------
            output$forecastConfirmedRaw <- renderPlot ({
                d2 <- ChinaHistory
                par(mar = c(4, 3, 0, 2))
                # missing data with average of neighbors
                d2$confirm<- meanImput(d2$confirm, 2)
                
                confirm <- ts(d2$confirm, # percent change
                                start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
                forecasted <- forecast(ets(confirm), input$daysForcasted)
                plot(forecasted, xaxt="n", main="", 
                     ylab = z("全国确诊"),
                     xlab = paste0(z("预期"), input$daysForcasted, z("天后全国确诊 "), round(forecasted$mean[input$daysForcasted],0), z(", 区间["),
                                  round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
                )
                a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
                axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
            }, width = plotWidth - 100 ) 
            
            #全国死亡人数预测, 用百分比预测-------------------------------------------
            output$forecastDeadChange <- renderPlot ({
                d2 <- ChinaHistory
                par(mar = c(4, 3, 0, 2))       
                
                # missing data with average of neighbors
                d2$dead <- meanImput(d2$dead, 2)
                
                d2 <- d2[-(1:20), ] # remove the first 10 days as % change is huge
                
                # Note that 5 is added to the denominator for stablize the %
                dead <- ts(diff(d2$dead)/(5 + d2$dead[1:(nrow(d2)-1)])*100, # percent change
                             start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )
            
            
                
                forecasted <- forecast(ets(dead), input$daysForcasted)
                
#                predictedNdeaded = d2$dead[nrow(d2)]* (1+ forecasted$mean[input$daysForcasted]/100)^input$daysForcasted 
                predictedNdead = d2$dead[nrow(d2)]* increasesByPercentages(forecasted$mean)           
                plot(forecasted, xaxt="n", main="", 
                     ylab = z("死亡人数增加百分比(%)"),
                     xlab = paste0(z("预期全国死亡累计每天增加"), round(mean(forecasted$mean),1),
                                  "%，", input$daysForcasted, z("天后达到 "), round(predictedNdead,0) )            
                )
                a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
                axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
            }, width = plotWidth - 100 )
            
            #全国死亡人数预测, 直接预测-------------------------------------------
            output$forecastDeadRaw <- renderPlot ({
                d2 <- ChinaHistory
                par(mar = c(4, 3, 0, 2))        
                # missing data with average of neighbors
                d2$dead <- meanImput(d2$dead, 2)
                
                deaded <- ts(d2$dead, # percent change
                             start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
                forecasted <- forecast(ets(deaded), input$daysForcasted)
                plot(forecasted, xaxt="n", main="", 
                     ylab = z("全国死亡人数"),
                     xlab = paste0(z("预期"), input$daysForcasted, z("天后全国死亡累计"), round(forecasted$mean[input$daysForcasted],0), z(", 区间["),
                                  round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
                )
                a = seq(as.Date(min(d2$time)), by="days", length= + nrow(d2) -1 )
                axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
            }, width = plotWidth - 100 )  
            
            #世界地图--------------------------------------------------
            output$worldMap <- renderPlot ({
                withProgress(message = z('下载地图'), value = 0, {
                incProgress(0.1)
                plot(y, 
                     continuous_scale=FALSE,
                     palette='Blues')
                })
            }, height = 800, width = 800)  
            
            #中国地图---------------------------------------------------
            output$ChinaMap <- renderPlot ({
                withProgress(message = z('下载地图'), value = 0, {
                incProgress(0.1)
                cn = get_map_china()
                cn$province <- trans_province(cn$province) 
                incProgress(0.5)
                })
                plot(get_nCov2019(lang='en'), region='china', chinamap=cn,
                     continuous_scale=FALSE,
                     palette='Blues')
            
            }, height = 800, width = 800)   
            
            #省地图---------------------------------------------------
            output$provinceMap <- renderPlot ({
                # 英语版或直辖市不画地图
               # if(isEnglish | input$selectProvince0 %in% specialProvinces) { 
              if(isEnglish ) { 
                  return(NULL)
                  } else { 
                    map1 = sf::st_read(system.file("shijie.shp", package="nCov2019"))  
                    plot(y, region = input$selectProvince0, 
                         chinamap = map1,
                        palette='Blues')  
                    }
            }, height = 600, width = 800)  
            
            
            output$dataDownload <- downloadHandler(
              filename = function() {paste0("coronavirus_histrical_",x$time,".tsv")},
              content = function(file) {
                # issues with Chinese characters solved
                # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
                con <- file(file, open = "w+", encoding = "native.enc")
                df <- x$data
                df$time <- as.character(format(df$time))
                writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
                for(i in 1:nrow( df) )
                  #write line by line 
                  writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
                close(con)
              }
            )
            
            output$dataDownloadWorld <- downloadHandler(
              filename = function() {paste0("coronavirus_histrical_",x$time,".tsv")},
              content = function(file) {
                # issues with Chinese characters solved
                # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
                con <- file(file, open = "w+", encoding = "native.enc")
                df <- x["global"]
                df$time <- as.character(format(df$time))
                writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
                for(i in 1:nrow( df) )
                  #write line by line 
                  writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
                close(con)
              }
            )
        
    
    }
    shinyApp(ui = ui, server = server)

    }
}        
            
    




