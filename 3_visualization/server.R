##########===== server =====##########
library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(ECharts2Shiny)
library(shinyWidgets)
library(DT)
library(tidyr)
library(magrittr)
library(ggrepel)
library(rgdal)

# windowsFonts(myFont=windowsFont("微软雅黑"))
server <- shinyServer(function(input, output){
  ########## 数据区 ##########
  originaldata1 <- reactive({ 
    award_data <- fread('D:/#R/GMCM/winner-info/award_data.csv',encoding='UTF-8', stringsAsFactors=T)
    award_data <- award_data[award_type %in% c('一等奖','二等奖','三等奖'),]
    award_data$award_type <- factor(award_data$award_type, levels=c('一等奖','二等奖','三等奖'),order=T)
    return(award_data)
  })
  originaldata2 <- reactive({ 
    continue_data <- fread('D:/#R/GMCM/winner-info/continue_data.csv', encoding='UTF-8', stringsAsFactors=T)
    continue_data$award_type <- factor(continue_data$award_type, levels=c('一等奖','二等奖','三等奖'),order=T)
    return(continue_data)
  })
  originaldata3 <- reactive({ 
    china_map  <- readOGR("D:/#R/GMCM/CHN_adm/bou2_4p.shp",stringsAsFactors=FALSE)
    mydata1    <- china_map@data %>% mutate(id = row.names(.)) %>% select(id,NAME)      #sp地图对象中的行政区划信息    
    china_map1 <- fortify(china_map)  %>% select(id,long,lat,group,order)               #sp地图对象中的多边形边界点信息
    china_map_data <- left_join(china_map1,mydata1,by = "id")  
    province_city <- fread('D:/#R/GMCM/winner-info/province_city.csv',encoding='UTF-8')
    return(list(x=china_map_data,y=province_city))
  })
  inputdata1 <- reactive({
    data_11 <- originaldata1() %>%
      .[,.(count=length(id)),by=.(year,award_type)] %>% 
      spread(key=award_type,value='count') %>%
      as.data.frame() %>%
      .[,2:4] %>%
      set_rownames(2004:2017) 
    data_12 <- originaldata1() %>%
      .[,.(count=length(id)),by=.(year,question_type)] %>% 
      spread(key=question_type,value='count') %>%
      as.data.frame() %>%
      .[,2:8] %>%
      set_rownames(2004:2017) 
    data_12[is.na(data_12)] <- 0
    return(list(x=data_11,y=data_12))
  })
  inputdata21 <- reactive({
    data_21 <- originaldata2()[,3:5] %>% 
      .[award_type %in% input$inp_21&year>=input$inp_22[1]&year<=input$inp_22,'affiliation'] %>% 
      table() %>%
      as.data.frame() %>% 
      arrange(.,desc(Freq)) %>% 
      data.table() %>%
      set_colnames(c('affiliation','Freq'))
    return(data_21)
  })
  inputdata22 <- reactive({
    tdata_22 <- originaldata2() %>%
      .[,4:6] %>% 
      .[award_type %in% input$inp_21&year>=input$inp_22[1]&year<=input$inp_22,3] %>% 
      table() %>% 
      as.data.frame() %>%
      set_colnames(c('NAME','Count'))
    data_22 <- left_join(originaldata3()$x,tdata_22,by = "NAME")
    data_22[is.na(data_22)] <- 0
    data_23 <- left_join(originaldata3()$y,tdata_22) 
    return(list(x=data_22,y=data_23))
  })
  
  inputdata3 <- reactive({
    tdata_31 <- originaldata2() %>% 
      .[year>=input$inp_33[1]&year<=input$inp_33[2]&award_type %in% input$inp_31,c(4,5,6,8,9)] %>%
      data.table() %>%
      arrange(.,member_unique,continue_time) %>%
      unique(by='member_unique',fromLast=T) %>%
      data.table() %>% 
      .[continue_time>=input$inp_32[1]&continue_time<=input$inp_32[2],3] %>%
      table() %>%
      as.data.frame() %>%
      set_colnames(c('NAME','Count'))
    data_31 <- left_join(originaldata3()$x,tdata_31,by = "NAME")
    data_31[is.na(data_31)] <- 0
    data_32 <- left_join(originaldata3()$y,tdata_31)
    return(list(x=data_31,y=data_32))
  })
  inputdata4 <- reactive({
    data_41 <- originaldata1() %>% 
      .[,c(10,1,3,4,5,6,7,8,9)]
    return(data_41)
  })
  
  ########## 模块区 ##########
  # By team
  renderLineChart(div_id="plot_11", data=inputdata1()$x, axis.x.name='年份', axis.y.name='获奖数量',theme='shine')
  renderBarChart(div_id="plot_12", data=inputdata1()$y, axis.x.name='年份', axis.y.name='获奖数量',theme='shine',
                 direction="vertical", grid_left="10%", stack_plot=T)
  # By member
  trans <- function(x){
    y1 <- y2 <- y3 <- NA
    if('一等奖' %in% x) y1 <- '一等奖'
    if('二等奖' %in% x) y2 <- '二等奖'
    if('三等奖' %in% x) y3 <- '三等奖'
    y <- paste(na.omit(c(y1,y2,y3)),collapse='、')
    return(y)
  }
  output$plot_21 <- renderPlot({
    ggplot(inputdata21()[1:input$inp_23,],aes(reorder(affiliation,Freq),Freq,label=Freq))+
      geom_bar(stat='identity',fill='#57ADF3',color='white')+
      geom_text(size=5,stat='identity',hjust=1,vjust=0.5)+
      scale_y_continuous(expand=expand_scale(mult=c(0,0.02)))+
      labs(x='院校名称',y='获奖数量',
           title=sprintf('获奖数量最多的前%d个培养单位',input$inp_23),
           caption=sprintf('统计区间：%d-%d年， 统计奖项：%s',input$inp_22[1],input$inp_22[2],trans(input$inp_21)))+
      coord_flip()+
      theme_light()+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=15),
            plot.title=element_text(size=25,hjust=0.5),
            plot.caption=element_text(size=10), 
            text=element_text(family='myFont'))
  })
  output$plot_22 <- renderPlot({
    trans <- function(x){
      y1 <- y2 <- y3 <- NA
      if('一等奖' %in% x) y1 <- '一等奖'
      if('二等奖' %in% x) y2 <- '二等奖'
      if('三等奖' %in% x) y3 <- '三等奖'
      y <- paste(na.omit(c(y1,y2,y3)),collapse='、')
      return(y)
    }
    ggplot()+
      geom_polygon(data=inputdata22()$x,aes(long,lat,group=group,fill = Count),colour="grey50")+
      geom_text_repel(data=inputdata22()$y,aes(x=lon, y=lat, label=paste(province,Count,sep=':')),size=5,
                      family= "myFont",fontface="bold") +
      coord_map("polyconic") +
      scale_fill_gradient2(low='#FFFFFF',high='#006D2C')+
      theme_void()+
      labs(title='各省获奖人数',
           caption=sprintf('统计区间：%d-%d年， 统计奖项：%s',input$inp_22[1],input$inp_22[2],trans(input$inp_21)))+
      theme(plot.title=element_text(size=25,hjust=0.5),
            plot.caption=element_text(size=10), 
            text=element_text(family='myFont'))
  })
  
  # Successive award
  output$plot_31 <- renderPlot({
    trans <- function(x){
      y1 <- y2 <- y3 <- NA
      if('一等奖' %in% x) y1 <- '一等奖'
      if('二等奖' %in% x) y2 <- '二等奖'
      if('三等奖' %in% x) y3 <- '三等奖'
      y <- paste(na.omit(c(y1,y2,y3)),collapse='、')
      return(y)
    }
    ggplot()+
      geom_polygon(data=inputdata3()$x,aes(long,lat,group=group,fill = Count),colour="grey50")+
      geom_text_repel(data=inputdata3()$y,aes(x=lon, y=lat, label=paste(province,Count,sep=':')),family= "myFont",fontface="bold") +
      coord_map("polyconic") +
      scale_fill_gradient2(low='#FFFFFF',high='#006D2C')+
      theme_void()+
      labs(title=sprintf('各省连续%d-%d次获奖的人数',input$inp_32[1],input$inp_32[2]),
           caption=sprintf('统计区间：%d-%d年， 统计奖项：%s',input$inp_33[1],input$inp_33[2],trans(input$inp_31)))+
      theme(plot.title=element_text(size=25,hjust=0.5),
            plot.caption=element_text(size=10), 
            text=element_text(family='myFont'))
  })
  
  # Team information
  output$table_41 <- renderDataTable({ 
    data_41 <- inputdata4() %>% data.table() 
    if(length(input$inp_41)!=0) data_41 <- data_41[year %in% input$inp_41,]
    if(length(input$inp_42)!=0) data_41 <- data_41[award_type %in% input$inp_42,]
    if(length(input$inp_43)!=0) data_41 <- data_41[question_type %in% input$inp_43,]
    datatable(data_41)
  })
  
})
