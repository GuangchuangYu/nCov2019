library(shiny)
library(tidyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(scales)
library(forecast) # time series
library(lubridate) # for showing up time correctly

ui <- dashboardPage(
  dashboardHeader(title = "nCov2019 Dashboard"),

  dashboardSidebar(
  
      # choose country
    selectizeInput(
      'country', '选择国家', choices = NULL,
      options = list(
        placeholder = '选择国家',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    

    # choose province 
    conditionalPanel('["中国", "韩国", "美国", "日本", "伊朗", "伊拉克", "德国", "英国"].indexOf(input.country) > -1', 
                    selectizeInput('province', '选择省份', 
                        choices = c("Select Province" =  NULL  ),
                        options = list( placeholder = '省份')),
                    tags$p("")),

    # choose city
    conditionalPanel('["中国"].indexOf(input.country) > -1', 
                    selectizeInput('city', '选择城市', 
                    choices = NULL,
                    options = list( placeholder = '城市')),
                    tags$p("")),
    #numericInput("num", "days to forecast ", min = 7, step = 1, value = 10),
    sliderInput("num", "days to forecast",
                          min = 1, max = 10,
                          value = 5),
    tags$div(
    tags$p("下载表格数据"),
    downloadButton('dataDownload', '下载'	),style = "padding: 12px 15px 0px 15px"
    )

    
    ),

  dashboardBody(
    
      # Create a page with fluid layout
    fluidRow(
      valueBoxOutput(outputId="summary_confirm"),
      valueBoxOutput(outputId="summary_cure"),
      valueBoxOutput(outputId="summary_dead")
    ),
    fluidRow(

      box(title = "Data Table",
          solidHeader = T,
          width = 4,
          collapsible = T,
           DT::dataTableOutput("data_table"), style = "font-size: 70%;"),

          box(title = "Plot", solidHeader = T,
          width = 8, collapsible = T,
          plotlyOutput("line_plot"))
    ), # row

    fluidRow(
      tabBox(
        width=12,
      title = "",
      selected = "Global Confirmed",
      tabPanel("Global Confirmed", plotOutput("worldwide_plot")),
      tabPanel("Global Mortality Rate", plotlyOutput("Mortality_plot")),
      tabPanel("Global Health Rate", plotlyOutput("Health_plot")),
      tabPanel("Country Statistics", plotOutput("country_plot")),
      tabPanel("Forecast", plotOutput("forecast"))
    )
    ) # end row
  ) # end dashboard body 

) # end UI

server <- function(input, output, session, ...) {

    # load data 
    data = load_nCov2019(lang='zh')
    t = data$time
    nn <- readRDS(system.file("country_translate.rds", package="nCov2019"))
    # update country list
    country_list <- filter(data$global, time == t) %>% 
            arrange(desc(cum_confirm)) %>% .$country
    updateSelectizeInput(session, 'country', choices = country_list, server = TRUE)
    
    # update province list
    observe({
        province <- unique(subset(data$province, country == input$country)$province)
        updateSelectInput(session, "province", choices = c("",province))
    })

    # update city list
    observe({
        city <- unique(subset(data$data, province == input$province)$city)
        updateSelectInput(session, "city", choices = c("",city))
    })

    # prepare the table content
    df <- reactive({
        x = data.frame()
        if ( nchar(input$country) > 0 ) {
            x = subset(data$global, country == input$country)
        }
        if ( nchar(input$province) > 0 ) {
            x = subset(data$province, province == input$province)
        }
        if ( nchar(input$city) > 0  ) {
        x =  subset(data$data, city == input$city)
        }
        x = x[,c("time","cum_confirm","cum_heal","cum_dead")]
        return(x)
    })

    num <- reactive({
    input$num
    })


  # output data table
    output$data_table = DT::renderDataTable({
        validate(need(input$country != "", "Loading"))
        df()
    },rownames = FALSE )


 # output header summary 
    output$summary_confirm <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$time == t),]$cum_confirm, " 确诊"), 
                    t, icon = icon("virus"), color = "yellow")
    })

    output$summary_cure <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$time == t),]$cum_heal, " 康复"), 
                    t, icon = icon("hospital"), color = "green")
    })

    output$summary_dead <- renderValueBox({
        validate(need(input$country != "", "Loading"))
        x = df()
        valueBox(
            paste0(x[which(x$time == t),]$cum_dead, " 死亡"), 
                    t, icon = icon("skull-crossbones"), color = "red")
})


    output$line_plot <- renderPlotly({
        validate(need(input$country != "", "Loading"))
        x = gather(df(), curve, count, -time)
        p = ggplot(x, aes(time, count, color = curve)) +
            geom_point() + geom_line() + xlab(NULL) + ylab(NULL) +
            scale_color_manual(values=c("#f39c12", "#dd4b39", "#00a65a")) +
            theme_bw() + 
            theme(legend.position = "none") +
                theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
                scale_x_date(date_labels = "%Y-%m-%d")
        ggplotly(p)
    })



# data download

    output$dataDownload <- downloadHandler(
      filename = function() {paste0("coronavirus_histrical_",t,".tsv")},
      content = function(file) {
        # issues with Chinese characters solved
        # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
        con <- file(file, open = "w+", encoding = "native.enc")
        df <- df()
        df$country = input$country
        df$province = input$province
        df$city = input$city
        df$time <- as.character(format(df$time))
        writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
        for(i in 1:nrow( df) )
          #write line by line 
          writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
        close(con)
      }
    )

# bottom panel plots

    # output worldwide_plot
    output$worldwide_plot <- renderPlot({
        validate(need(input$country != "", "Loading"))
        plot(data)
    })

    # output country_plot
    output$country_plot <- renderPlot({
        validate(need(input$country != "", "Loading"))
        country_eng  <- nn[as.character(input$country)]
        country_eng <- sub("United\\sStates.*", "USA", country_eng)
        country_eng <- sub("Republic\\sof\\sKorea", "South Korea", country_eng)
        country_eng <- sub("United\\sKingdom.*", "UK", country_eng)
        country_eng <- sub("Republika\\sSeverna\\sMakedonija", "Macedonia", country_eng)
        if(country_eng == 'China'){country_eng = c('China','Taiwan')}
        plot(data, region = country_eng, date=t)

    })

    # top country plots
    output$Mortality_plot <- renderPlotly({

        d = data$global
        df <- filter(d, time == t) %>% 
        arrange(desc(cum_confirm)) 
        df = df[1:100, ]

        df$rate = df$cum_dead/df$cum_confirm

        df <- df %>% 
        arrange(desc(rate)) 

        df$country = factor(df$country, levels=df$country)
        percent <- function(x, digits = 2, format = "f", ...) {
            paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
            }

        df$Mortality <- percent(df$rate)
        df2 = df[order(df$cum_dead,decreasing = T),][1:30,]

        p <- ggplot(df, aes(x = country, y = rate, color = rate ,label = Mortality, confirm = cum_confirm )) +
            geom_point(aes(size=cum_confirm), alpha = .65) + 
            scale_color_gradientn(colors=c("darkgreen", "orange", "firebrick","red")) +
            labs(title = "COVID-19 Mortality Rate") + theme_bw() + 
            theme(legend.position = "none") + xlab(NULL) + ylab('Mortality Rate') + 
            scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
            scale_size(range=c(1,20)) + theme(axis.text.x = element_text(angle=45, hjust=1))

            ggplotly(p,tooltip = c("x","label","confirm"))

            })

    # top country plots
    output$Health_plot <- renderPlotly({

        d = data$global
        df <- filter(d, time == t) %>% 
        arrange(desc(cum_confirm)) 
        df = df[1:100, ]

        df$rate = df$cum_heal/df$cum_confirm
        df <- df %>% 
        arrange(desc(rate)) 

        df$country = factor(df$country, levels=df$country)
        percent <- function(x, digits = 2, format = "f", ...) {
            paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
            }

        df$Health <- percent(df$rate)

        p <- ggplot(df, aes(x = country, y = rate, color = rate ,label = Health, Health = cum_heal )) +
            geom_point(aes(size=cum_heal), alpha = .65) + 
            scale_color_gradientn(colors=c("firebrick","orange","darkgreen","green")) +
            labs(title = "COVID-19 Health Rate") + theme_bw() + 
            theme(legend.position = "none") + xlab(NULL) + ylab('Health Rate') + 
            scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
            scale_size(range=c(1,20)) + theme(axis.text.x = element_text(angle=45, hjust=1))
        ggplotly(p,tooltip = c("x","label","Health"))
})


    output$forecast <- renderPlot ({
        d2 <- df()
        options(scipen=999)
        options(warn=-1)
        par(mar = c(4, 3, 0, 2))
        confirm <- ts(d2$cum_confirm, # percent change
                        start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=100  )
        forecasted <- forecast(ets(confirm), num())
        plot(forecasted, xaxt="n", main="") + ylim(0,NA)

    }) 

### end
}

shinyApp(ui = ui, server = server)

