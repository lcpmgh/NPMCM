
########################################################
server <- function(input, output){
  ########## 1. 运行准备 ##########
  # 1.1 函数准备
  my_read_csv <- function(file_name, ...){
    data <- fread(paste0('./data/', file_name), encoding = 'UTF-8', ...)
    return(data)
  }
  trans <- function(x){
    # 为图例命名时，按动态选择进行更改用
    y1 <- y2 <- y3 <- NA_character_
    if('一等奖' %in% x) y1 <- '一等奖'
    if('二等奖' %in% x) y2 <- '二等奖'
    if('三等奖' %in% x) y3 <- '三等奖'
    y <- paste(na.omit(c(y1, y2, y3)), collapse = '、')
    return(y)
  }
  # 1.2 编程环境
  font_add("myFont", './data/0-typeface.ttc')    #加载中文字体
  showtext_auto()
  
  
  ########## 2. 数据区 ########## 
  ##### 2.1 函数准备 #####
  # 2.0.1 【读取】获奖队伍数据
  read_data_team <- reactive({
    data_team <- my_read_csv('2-total_data.csv') %>% 
      .[, A_type:=factor(A_type, levels = c('一等奖','二等奖','三等奖','成功参与奖'), order = T)]
    return(data_team)
  })
  # 2.0.2 【读取】获奖成员数据
  read_data_member <- reactive({
    data_member <- my_read_csv('5-member_data.csv') %>% 
      .[, A_type:=factor(A_type, levels = c('一等奖','二等奖','三等奖'), order = T)]
    return(data_member)
  })
  # 2.03 【读取】地图数据
  read_map_data <- reactive({
    china_map   <- readOGR('./data/0-Chinesemap.shp', stringsAsFactors=FALSE)
    mydata1     <- china_map@data %>% 
      as.data.table() %>% 
      .[, id:=(as.numeric(row.names(.))-1)] %>% 
      .[, c('id', 'NAME')]        #sp地图对象中的行政区划信息，生成这两个变量代码有点冗余，主要是为了统一用DT，避免加载额外包    
    china_map1  <- fortify(china_map) %>% 
      as.data.table() %>% 
      .[, c(6,1,2,7,3)] %>% 
      .[, id:=as.numeric(id)]     #sp地图对象中的多边形边界点信息，生成这两个变量代码有点冗余，主要是为了统一用DT，避免加载额外包   
    china_map_data <- china_map1[mydata1, on = 'id']
    province_city <- my_read_csv('0-province_capital_location.csv') 
    return(list(y1 = china_map_data, y2 = province_city))
  })
  
  
  ##### 2.1 第一页 #####
  # 2.1.1 创建【图1-1】所需数据
  create_data11 <- reactive({
    data11 <- read_data_team() %>% 
      dcast(Year~A_type, value.var = 'Year', fun = length) %>% 
      .[,s:=apply(.SD, 1, sum), .SDcols=2:5] %>% 
      .[, round(.SD/s*100,2), .SDcol=2:5, by='Year'] %>% 
      .[, s:=100-.SD, .SDcols=5] %>% 
      .[s==100, c('一等奖','二等奖','三等奖','s'):=.(NA,NA,NA,NA)] %>% 
      as.data.frame() %>% 
      set_rownames(.,.[,1]) %>% 
      dplyr::rename('综合获奖率'=s) %>% 
      .[,c(2,3,4,6)]
    return(data11)
  })
  # 2.1.2 创建【图1-2】所需数据
  create_data12 <- reactive({
    data12 <- read_data_team() %>% 
      .[A_type!='成功参与奖',] %>% 
      dcast(Year~A_type, value.var = 'Year', fun = length) %>% 
      .[, I('总获奖'):=sum(get('一等奖'), get('二等奖'), get('三等奖')), by=.(Year)] %>% 
      as.data.frame() %>% 
      set_rownames(.,.[,1]) %>% 
      .[, -1] 
    return(data12)
  })
  # 2.1.3 创建【图1-3】所需数据
  create_data13 <- reactive({
    data13 <- read_data_team() %>% 
      .[A_type!='成功参与奖',] %>% 
      dcast(Year~Q_type, value.var = 'Year', fun = length) %>% 
      as.data.frame() %>% 
      set_rownames(.,.[,1]) %>% 
      .[, -1] 
    return(data13)
  })
  # 2.1.4 创建【图1-4】所需数据
  create_data14 <- reactive({
    data14 <- read_data_team() %>% 
      .[A_type!='成功参与奖',] %>% 
      dcast(Year~Q_type, value.var = 'Year', fun = length) %>% 
      as.data.frame() %>% 
      set_rownames(.,.[,1]) %>% 
      .[, -1] %>% 
      divide_by(apply(.,1,sum)) %>% 
      multiply_by(100) %>% 
      round(2)
    return(data14)
  })
  
  
  ##### 2.2 第二页 #####
  # 2.2.1 创建【图2-1】数据
  create_data21 <- reactive({
    data21 <- read_data_member() %>% 
      .[, c('Unit', 'Year', 'A_type')] %>% 
      .[A_type %in% input$inp_21&Year>=input$inp_22[1]&Year<=input$inp_22, ] %>% 
      .[, .(Freq = length(Year)), by = .(Unit)] %>% 
      setorder(-Freq) %>% 
      .[1:input$inp_23, ]
    return(data21)
  })
  # 2.2.2 创建【图2-2】数据
  create_data22 <- reactive({
    map_data <- read_map_data()
    china_map_data <- map_data$y1
    province_city <- map_data$y2
    data22 <- read_data_member() %>%
      .[, c('Year','A_type','Province')] %>% 
      .[A_type %in% input$inp_21&Year>=input$inp_22[1]&Year<=input$inp_22[2], ] %>% 
      .[Province!='国外', .(Count=length(Year)), by = .(Province)] %>% 
      set_colnames(c('NAME','Count'))
    data22_1 <- data22[china_map_data, on = "NAME"] %>% .[is.na(Count), Count:=0] 
    data22_2 <- data22[province_city, on='NAME'] %>% .[is.na(Count), Count:=0]
    return(list(y1 = data22_1, y2 = data22_2))
  })
  
  
  ##### 2.3 第三页 #####
  # 2.3.1 创建图【3-1】和【图3-2】共同数据
  create_data3 <- reactive({
    data3 <- read_data_member() %>%
      .[, c('M_unique', 'Name','Series', 'Year', 'Unit', 'Province')] %>% 
      .[Series>=input$inp_31[1]&Series<=input$inp_31[2]&Year>=input$inp_32[1]&Year<=input$inp_32[2], ] %>% 
      setorder(., M_unique, Year) %>% 
      .[!duplicated(M_unique, fromLast = T), ]  
    return(data3)
  })
  # 2.3.2 创建【图3-1】数据
  create_data31 <- reactive({
    data31 <- create_data3() %>% 
      .[, .(Freq=length(M_unique)), by = .(Unit)] %>% 
      setorder(-Freq) %>% 
      .[1:input$inp_33, ] %>% 
      .[!is.na(Unit), ]
    return(data31)
  })
  # 2.3.3 创建【图3-2】数据
  create_data32 <- reactive({
    map_data <- read_map_data()
    china_map_data <- map_data$y1
    province_city <- map_data$y2
    data32 <- create_data3() %>% 
      .[, .(count=length(M_unique)), by = .(Province)] %>% 
      set_colnames(c('NAME', 'Count'))
    data32_1 <- data32[china_map_data, on = "NAME"] %>% .[is.na(Count), Count:=0]
    data32_2 <- data32[province_city, on='NAME'] %>% .[is.na(Count), Count:=0]
    return(list(y1 = data32_1, y2 = data32_2))
  })
  
  
  ##### 2.4 第四页 #####
  # 2.4.1 创建【表4】数据
  create_data4 <- reactive({
    data4 <- read_data_team() %>% 
      .[,c('Year', 'Q_type', 'C_name', 'C_unit', 'F_name', 'F_unit', 'S_name', 'S_unit', 'A_type')]
    if(length(input$inp_41)!=0) data4 <- data4[Year   %in% input$inp_41,]
    if(length(input$inp_42)!=0) data4 <- data4[A_type %in% input$inp_42,]
    if(length(input$inp_43)!=0) data4 <- data4[Q_type %in% input$inp_43,]
    return(data4)
  })
  
  
  ########## 3. 模块区 ##########
  ##### 3.1 第一页 #####
  # 3.1.1 【图3-1】
  renderLineChart(div_id = "plot_11", 
                  data = create_data11(), 
                  axis.x.name = '年份', 
                  axis.y.name = '百分数(%)', 
                  theme = 'shine')
  # 3.1.2 【图3-2】
  renderLineChart(div_id = "plot_12", 
                  data = create_data12(), 
                  axis.x.name = '年份', 
                  axis.y.name = '数量', 
                  theme = 'shine')
  # 3.1.3 【图3-3】
  renderBarChart(div_id = "plot_13", 
                 data = create_data13(), 
                 axis.x.name = '年份', 
                 axis.y.name = '数量', 
                 theme='shine',
                 direction = "vertical", 
                 grid_left = "10%",
                 stack_plot = T)
  # 3.1.4 【图3-4】
  renderBarChart(div_id = "plot_14", 
                 data = create_data14(), 
                 axis.x.name = '年份', 
                 axis.y.name = '百分数(%)', 
                 theme='infographic',
                 direction = "vertical", 
                 grid_left = "10%",
                 stack_plot = T)
  
  
  ##### 3.2 第二页 #####
  # 3.2.1 【图2-1】，获奖人次最多培养单位
  output$plot_21 <- renderPlot({
    ggplot(create_data21(), aes(reorder(Unit, Freq), Freq, label = Freq))+
      geom_bar(stat = 'identity', fill = '#57ADF3', color = 'white')+
      geom_text(size = 5, stat = 'identity', hjust = 1, vjust = 0.5)+
      scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
      labs(x = '院校名称', y = '人次',
           title = sprintf('获奖人次最多的%d个培养单位', input$inp_23),
           caption = sprintf('统计区间：%d-%d年， 统计奖项：%s', input$inp_22[1], input$inp_22[2], trans(input$inp_21)))+
      coord_flip()+
      theme_light()+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 25, hjust = 0.5),
            plot.caption = element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  # 3.2.2 【图2-2】，各省获奖人次
  output$plot_22 <- renderPlot({
    data22 <- create_data22()
    ggplot()+
      geom_polygon(data = data22$y1, aes(long, lat, group = group, fill = Count), colour="grey50")+
      geom_text_repel(data = data22$y2, aes(x = Lon, y = Lat, label = paste(Province, Count, sep = ':')), size=5,
                      family = "myFont", fontface="bold") +
      coord_map("polyconic") +
      scale_fill_gradient2(low = '#FFFFFF', high = '#006D2C')+
      theme_void()+
      labs(title = '各省获奖人次', fill = '人次',
           caption = sprintf('统计区间：%d-%d年， 统计奖项：%s',input$inp_22[1],input$inp_22[2],trans(input$inp_21)))+
      theme(plot.title=element_text(size = 25, hjust = 0.5),
            plot.caption=element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  
  
  ##### 3.3 第三页 #####
  # 3.3.1 【图3-1】，获奖最多培养单位
  output$plot_31 <- renderPlot({
    ggplot(create_data31(), aes(reorder(Unit, Freq), Freq, label = Freq))+
      geom_bar(stat = 'identity', fill = '#57ADF3', color = 'white')+
      geom_text(size = 5, stat = 'identity', hjust = 1, vjust = 0.5)+
      scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
      labs(x = '院校名称', y = '人数',
           title = sprintf('连续%d-%d次获奖人数最多的%d个培养单位', input$inp_31[1], input$inp_31[2], input$inp_33),
           caption = sprintf('统计区间：%d-%d年， 统计次数：%d-%d', input$inp_32[1], input$inp_32[2], input$inp_31[1], input$inp_31[2]))+
      coord_flip()+
      theme_light()+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 25, hjust = 0.5),
            plot.caption = element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  # 3.3.2 【图3-2】，连续获奖数量分布
  output$plot_32 <- renderPlot({
    data32 <- create_data32()
    ggplot()+
      geom_polygon(data = data32$y1, aes(long, lat, group = group, fill = Count), colour="grey50")+
      geom_text_repel(data = data32$y2, aes(x = Lon, y = Lat, label = paste(Province, Count, sep = ':')), size=5,
                      family = "myFont", fontface="bold") +
      coord_map("polyconic") +
      scale_fill_gradient2(low = '#FFFFFF', high = '#006D2C')+
      theme_void()+
      labs(title = sprintf('各省连续%d-%d次获奖人数', input$inp_31[1], input$inp_31[2]), fill='人数',
           caption = sprintf('统计区间：%d-%d年， 统计次数：%d-%d',input$inp_32[1], input$inp_32[2], input$inp_31[1], input$inp_31[2]))+
      theme(plot.title=element_text(size = 25, hjust = 0.5),
            plot.caption=element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  
  
  ##### 3.4 第四页 #####
  # 3.4.1 【表4】，查询系统
  output$table_41 <- renderDataTable({ 
    data41 <- create_data4()
    datatable(data41)
  })
}