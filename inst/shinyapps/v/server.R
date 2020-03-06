# for plotting
library(ggplot2)
require(ggrepel)
library(tidyr) # for gather function
library(forcats) # ploting
library(forecast) # time series
library(lubridate) # for showing up time correctly
library(plotly)
library(chinamap)
library(maps)

function(input, output, session) {
    
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
        arrange( desc(time, city))  %>%
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
    
    #各个城市死亡率  -------------------------------------------
    output$deathRatesCitiesCountries <- renderPlotly({
      d = x$data %>% 
        arrange( desc(time, city))  %>%
        filter(!duplicated(city)) %>% 
        filter(province != city ) %>%
        filter (city != "监狱系统") %>%
       # filter(cum_dead > 2)  %>%
        filter(cum_confirm > 200) %>%
        mutate(rate = 100*cum_dead/cum_confirm) %>%
        arrange(desc(province), city) 

      d <- rbind(d, d[1,]) # move Hunan to the end
      d <- d[-1, ]
        
      
      if(isEnglish) d$province <- py2( d$province )  # translate into Pinyin      
      if(isEnglish) 
      d$city <- py3( d$city )
      
      d <- d %>% 
        mutate(name = paste(d$province, d$city) )  %>%
        mutate(name = factor(name, levels= rev(name)) )
      
      p <- ggplot(d, aes(x=name, y=rate, color = province)) +
      geom_segment( aes(xend=name, yend=0)) +
      geom_point( size=4, aes( color=province) ) +
      coord_flip() +
      theme_bw() +
      xlab("") +
      ylab(z("死亡率(%)")) + 
      theme(legend.position = "none")
      

      ggplotly(p, tooltip = c("y", "x")) %>% 
        layout( width = plotWidth)
      
    } ) 
    
    #世界各国死亡率，现在的数据 -------------------------------------------
    output$WorldDeathRate <- renderPlotly({
      d <- y['global',] %>%
        filter(!is.na(name)) %>%
        mutate( confirm =as.numeric(confirm) ) %>%
        #mutate (name = z2( name ) ) %>%
        mutate( name = fct_reorder(name, confirm)) %>% 
        filter( confirm > 200) %>%
        mutate(deadRate = as.numeric(deadRate))
      
      p <- ggplot(d, aes(x=name, y=deadRate)) +
        geom_segment( aes(xend=name, yend=0)) +
        geom_point( size=4, color = "orange" ) +
        coord_flip() +
        theme_bw() +
        ylab(z("死亡率(%)")) + 
        xlab("")
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = c("y", "x")) %>% 
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
             xlab = paste0(z("预期"), input$daysForcasted, z("天后确诊 "),input$selectCountry, " ", round(forecasted$mean[input$daysForcasted],0), z(", 区间["),
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
        
#        predictedNdeaded = d2$dead[nrow(d2)]* (1+ forecasted$mean[input$daysForcasted]/100)^input$daysForcasted 
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

    shijie <- get_city_map()

    #省地图---------------------------------------------------
    output$provinceMap <- renderPlot ({
        # 英语版或直辖市不画地图
       # if(isEnglish | input$selectProvince0 %in% specialProvinces) { 
      #if(isEnglish | !exists("shijie", envir=e1)) {
      #if(isEnglish | !is.null(shijie)) {  
      if(isEnglish | !exists("shijie")) {       
          return(NULL)
          } else {
            #shijie <- get("shijie",envir=e1)          
            plot(y, region = input$selectProvince0, 
                 chinamap = shijie,
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
        df <- xgithub["global"]
        df$time <- as.character(format(df$time))
        writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
        for(i in 1:nrow( df) )
          #write line by line 
          writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
        close(con)
      }
    )
    

}
