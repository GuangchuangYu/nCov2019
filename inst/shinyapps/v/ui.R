# Shiny app for showing the number of confirmed coronavirus cases across China
# Xijin Ge 2/5/2020

library(shiny)
library(plotly)
library(shinyBS,verbose=FALSE) # for popup figures
# Define server logic required to draw a histogram
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
#      ,column(2, downloadButton('dataDownload', z('下载')	) )
      ,column(6, align="right",a(z("英语"),  href=myURL) )
      )
    
    #,plotlyOutput("historicalChinaDataPlotly")
    ,img(src='ChinaMapAnimated2.gif', align = "center",width="600", height="450")
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
    ,tags$head(includeScript("ga.js")) # tracking usage with Google analytics      
)