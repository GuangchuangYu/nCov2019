library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(shinydashboard)
library(plotly)
library(chinamap)
library(forcats) # ploting
library(forecast) # time series
library(lubridate) # for showing up time correctly



ui <- dashboardPage(
  dashboardHeader(title = "nCov2019 Dashboard"),

  # 侧边作为统一的控制板
  dashboardSidebar(
  
      # 选择国家
    selectizeInput(
      'country', 'Choose Country', choices = NULL,
      options = list(
        placeholder = 'Choose Country',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    

    # 如果选择的国家有进一步的数据 province 
    conditionalPanel('["China", "South Korea", "United States", "Japan", "Iran", "Italy", "Germany", "United Kingdom"].indexOf(input.country) > -1', 
                    selectizeInput('province', 'Choose Province', 
                        choices = c("Select Province" =  NULL  ),
                        options = list( placeholder = 'Province')),
                    tags$p("")),

    # 如果选择的国家有进一步的数据  city
    conditionalPanel('["China"].indexOf(input.country) > -1', 
                    selectizeInput('city', 'Choose City', 
                    choices = NULL,
                    options = list( placeholder = 'City')),
                    tags$p("")),
    #numericInput("num", "days to forecast ", min = 7, step = 1, value = 10),
    sliderInput("num", "days to forecast",
                          min = 1, max = 10,
                          value = 5),
    tags$div(
    tags$p("Download chosen data"),
    downloadButton('dataDownload', 'Download'	),style = "padding: 12px 15px 0px 15px"
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
      title = "Map",
      selected = "Worldwide view",
      tabPanel("Worldwide view", plotOutput("worldwide_plot")),
      tabPanel("Country view", plotOutput("country_plot")),
      tabPanel("Top 30 countries view", plotOutput("top_plot")),
      tabPanel("forecast", plotOutput("forecast"))
    )
    ) # row
  ) # body

)

server <- function(input, output, session) {
# get data and country list
data = load_nCov2019(lang='en')
t = data$time
#country_list = unique(data$global$country )
country_list <- filter(data$global, time == t) %>% 
          arrange(desc(cum_confirm)) %>% .$country
updateSelectizeInput(session, 'country', choices = country_list, server = TRUE)
 
  #  update  province list
  observe({
    province <- unique(subset(data$province, country == input$country)$province)
    updateSelectInput(session, "province", choices = c("",province))
  })
  #  update city list
    observe({
    city <- unique(subset(data$data, province == input$province)$city)
    updateSelectInput(session, "city", choices = c("",city))
  })

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
       df()
  },rownames = FALSE )


 # output summary 
   output$summary_confirm <- renderValueBox({
        x = df()
    valueBox(
        paste0(x[which(x$time == t),]$cum_confirm, " confirm"), 
             t, icon = icon("virus"), color = "yellow")
  })

   output$summary_cure <- renderValueBox({
       x = df()
    valueBox(
        paste0(x[which(x$time == t),]$cum_heal, " health"), 
             t, icon = icon("hospital"), color = "green")
  })

    output$summary_dead <- renderValueBox({
    x = df()
valueBox(
    paste0(x[which(x$time == t),]$cum_dead, " dead"), 
            t, icon = icon("skull-crossbones"), color = "red")
})


# output plot

  output$worldwide_plot <- renderPlot({
    plot(data)
  })


  output$line_plot <- renderPlotly({
    x = gather(df(), curve, count, -time)
    p = ggplot(x, aes(time, count, color = curve)) +
  geom_point() + geom_line() + xlab(NULL) + ylab(NULL) +
  scale_color_manual(values=c("#f39c12", "#dd4b39", "#00a65a")) +
  theme_bw() + theme(legend.position = "none") +
  geom_text_repel(aes(label = curve), 
    data = x[x$time == t, ], hjust = 1) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d")
    ggplotly(p)
  })

  output$country_plot <- renderPlot({
    country_eng = input$country
    country_eng <- sub("United\\sStates.*", "USA", country_eng)
    country_eng <- sub("Republic\\sof\\sKorea", "South Korea", country_eng)
    country_eng <- sub("United\\sKingdom.*", "UK", country_eng)
    country_eng <- sub("Republika\\sSeverna\\sMakedonija", "Macedonia", country_eng)
    plot(data, region = country_eng, date=t)
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


    # 
      output$top_plot <- renderPlot({
          d = data$global
          dd <- filter(d, time == t) %>% 
          arrange(desc(cum_confirm)) 
          dd = dd[1:30, ]
          dd$country = factor(dd$country, levels=dd$country)
          dd$angle = 1:30 * 360/30
          cutoff = median(dd$cum_confirm)
        ggplot(dd, aes(country, cum_confirm, fill=cum_confirm)) + 
            geom_col(width=1, color='grey90') + 
            geom_col(aes(y=I(5)), width=1, fill='grey90', alpha = .2) +       
            geom_col(aes(y=I(3)), width=1, fill='grey90', alpha = .2) +    
            geom_col(aes(y=I(2)), width=1, fill = "white") +
            scale_y_log10() + 
            scale_fill_gradientn(colors=c("darkgreen", "green", "orange", "firebrick","red"), trans="log") + 
            geom_text(aes(label=paste(country,sep="\n"), 
                          y = cum_confirm *.8, angle=angle-90), 
                    data=function(d) d[d$cum_confirm >= cutoff,], 
                    size=3, color = "black", fontface="bold", vjust=1)  + 
            geom_text(aes(label=paste(country,sep="\n"), 
                          y = max(cum_confirm) * 2, angle=angle+90), 
                    data=function(d) d[d$cum_confirm < cutoff,], 
                    size=3, vjust=0) + 
            coord_polar(direction=-1) + 
            theme_void() + 
            theme(legend.position="none") +
            ggtitle("COVID19 global trend", t)
              })


      output$forecast <- renderPlot ({
        d2 <- df()
        options(scipen=999)
        options(warn=-1)
        par(mar = c(4, 3, 0, 2))
        # missing data with average of neighbors
        # d2$confirm<- meanImput(d2$cum_confirm, 2)
        
        confirm <- ts(d2$cum_confirm, # percent change
                      start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=100  )
        forecasted <- forecast(ets(confirm), num())

        plot(forecasted, xaxt="n", main="") + ylim(0,NA)
        #a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        #axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
      }) 

### end
}



shinyApp(ui = ui, server = server)

