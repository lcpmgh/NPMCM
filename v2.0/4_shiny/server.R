
########################################################
server <- function(input, output){
  ########## 函数准备 ##########
  file_dir <- function(name = NULL){paste0('D:/#R/GMCM/v2.0/data-GB2312/', name)}
  trans <- function(x){
    # 为图例命名时，按动态选择进行更改用
    y1 <- y2 <- y3 <- NA_character_
    if('一等奖' %in% x) y1 <- '一等奖'
    if('二等奖' %in% x) y2 <- '二等奖'
    if('三等奖' %in% x) y3 <- '三等奖'
    y <- paste(na.omit(c(y1, y2, y3)), collapse = '、')
    return(y)
  }
  # windowsFonts(myFont=windowsFont("微软雅黑"))
  showtext_auto()
  font_add("myFont", file_dir('D:/#R/GMCM/v2.0/data-GB2312/front/msyh.ttc')) # 你的中文字体位置
  
  ########## 数据区 ##########
  read_data_team <- reactive({
    # 读取获奖队伍数据
    data_team <- fread(file_dir('award_team.csv')) %>% .[, award_type:=factor(award_type, levels = c('一等奖','二等奖','三等奖'), order = T)]
    return(data_team)
  })
  
  read_data_member <- reactive({
    # 读取获奖队伍数据
    data_member <- fread(file_dir('award_member.csv')) %>% .[, award_type:=factor(award_type, levels = c('一等奖','二等奖','三等奖'), order = T)]
    return(data_member)
  })
  
  read_map_data <- reactive({
    china_map   <- readOGR(file_dir('mapdata/bou2_4p.shp'), stringsAsFactors=FALSE)
    mydata1     <- china_map@data %>% 
      as.data.table() %>% 
      .[, id:=(as.numeric(row.names(.))-1)] %>% 
      .[, c('id', 'NAME')]        #sp地图对象中的行政区划信息，生成这两个变量代码有点冗余，主要是为了统一用DT，避免加载额外包    
    china_map1  <- fortify(china_map) %>% 
      as.data.table() %>% 
      .[, c('id', 'long', 'lat', 'group', 'order')] %>% 
      .[, id:=as.numeric(id)]     #sp地图对象中的多边形边界点信息，生成这两个变量代码有点冗余，主要是为了统一用DT，避免加载额外包   
    china_map_data <- china_map1[mydata1, on = 'id']
    province_city <- fread(file_dir('province_capital.csv')) 
    return(list(y1 = china_map_data, y2 = province_city))
  })
  
  create_data1 <- reactive({
    # 创建图1数据
    year_range <- unique(read_data_team()$year) 
    # 图1-1数据
    data1 <- read_data_team() %>% 
      dcast(year~award_type, value.var = 'year', fun = length) %>% 
      .[, I('总获奖'):=sum(get('一等奖'), get('二等奖'), get('三等奖')), by=.(year)] %>% 
      as.data.frame() %>% 
      set_rownames(year_range) %>% 
      .[, -1]
    # 图1-2数据
    data2 <- read_data_team() %>% 
      dcast(year~question_type, value.var = 'year', fun = length) %>% 
      as.data.frame() %>% 
      set_rownames(year_range) %>% 
      .[, -1]
    return(list(y1 = data1, y2 = data2))
  })
  
  create_data21 <- reactive({
    # 创建图2-1数据
    data_21 <- read_data_member() %>% 
      .[, c('affiliation', 'year', 'award_type')] %>% 
      .[award_type %in% input$inp_21&year>=input$inp_22[1]&year<=input$inp_22, ] %>% 
      .[, .(Freq = length(year)), by = .(affiliation)] %>% 
      setorder(-Freq) %>% 
      .[1:input$inp_23, ]
    return(data_21)
  })
  
  create_data22 <- reactive({
    # 创建图2-2数据
    map_data <- read_map_data()
    china_map_data <- map_data$y1
    province_city <- map_data$y2
    data_22 <- read_data_member() %>%
      .[, c('year','award_type','province')] %>% 
      .[award_type %in% input$inp_21&year>=input$inp_22[1]&year<=input$inp_22[2], ] %>% 
      .[province!='国外', .(count=length(year)), by = .(province)] %>% 
      set_colnames(c('NAME','Count'))
    data_22_1 <- data_22[china_map_data, on = "NAME"] %>% .[is.na(Count), Count:=0]
    data_22_2 <- data_22[province_city, on='NAME'] %>% .[is.na(Count), Count:=0]
    return(list(y1 = data_22_1, y2 = data_22_2))
  })
  
  create_data3 <- reactive({
    # 创建图3-1和图3-2共同数据
    data_3 <- read_data_member() %>%
      .[, c('member_unique', 'name','series', 'year', 'affiliation', 'province')] %>% 
      .[series>=input$inp_31[1]&series<=input$inp_31[2]&year>=input$inp_32[1]&year<=input$inp_32[2], ] %>% 
      setorder(., member_unique, year) %>% 
      .[!duplicated(member_unique, fromLast = T), ]  
    return(data_3)
  })
  
  create_data31 <- reactive({
    # 创建图3-1数据
    data_31 <- create_data3() %>% 
      .[, .(Freq=length(member_unique)), by = .(affiliation)] %>% 
      setorder(-Freq) %>% 
      .[1:input$inp_33, ] %>% 
      .[!is.na(affiliation), ]
    return(data_31)
  })
  
  create_data32 <- reactive({
    # 创建图3-2数据
    map_data <- read_map_data()
    china_map_data <- map_data$y1
    province_city <- map_data$y2
    data_32 <- create_data3() %>% 
      .[, .(count=length(member_unique)), by = .(province)] %>% 
      set_colnames(c('NAME', 'Count'))
    data_32_1 <- data_32[china_map_data, on = "NAME"] %>% .[is.na(Count), Count:=0]
    data_32_2 <- data_32[province_city, on='NAME'] %>% .[is.na(Count), Count:=0]
    return(list(y1 = data_32_1, y2 = data_32_2))
  })
  
  create_data4 <- reactive({
    # 创建第四部分数据
    data_4 <- read_data_team() %>% 
      .[,c('year', 'question_type', 'c_name', 'c_affiliation', 'f_name', 'f_affiliation', 's_name', 's_affiliation', 'award_type')]
    if(length(input$inp_41)!=0) data_4 <- data_4[year %in% input$inp_41,]
    if(length(input$inp_42)!=0) data_4 <- data_4[award_type %in% input$inp_42,]
    if(length(input$inp_43)!=0) data_4 <- data_4[question_type %in% input$inp_43,]
    return(data_4)
  })
  
  ########## 模块区 ##########
  ##### 第一部分 #####
  renderLineChart(div_id = "plot_11", data = create_data1()$y1, axis.x.name = '年份', axis.y.name = '获奖数量', theme = 'shine')
  renderBarChart(div_id = "plot_12", data = create_data1()$y2, axis.x.name = '年份', axis.y.name = '获奖数量', theme='shine',
                 direction = "vertical", grid_left = "10%", stack_plot=T)
  
  ##### 第二部分 #####
  output$plot_21 <- renderPlot({
    # 图2-1，获奖最多培养单位
    ggplot(create_data21(), aes(reorder(affiliation, Freq), Freq, label = Freq))+
      geom_bar(stat = 'identity', fill = '#57ADF3', color = 'white')+
      geom_text(size = 5, stat = 'identity', hjust = 1, vjust = 0.5)+
      scale_y_continuous(expand = expand_scale(mult = c(0,0.02)))+
      labs(x = '院校名称', y = '获奖数量',
           title = sprintf('获奖数量最多的前%d个培养单位', input$inp_23),
           caption = sprintf('统计区间：%d-%d年， 统计奖项：%s', input$inp_22[1], input$inp_22[2], trans(input$inp_21)))+
      coord_flip()+
      theme_light()+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 25, hjust = 0.5),
            plot.caption = element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  output$plot_22 <- renderPlot({
    # 图2-2，各省获奖数量
    data_22 <- create_data22()
    ggplot()+
      geom_polygon(data = data_22$y1, aes(long, lat, group = group, fill = Count), colour="grey50")+
      geom_text_repel(data = data_22$y2, aes(x = lon, y = lat, label = paste(province, Count, sep = ':')), size=5,
                      family = "myFont", fontface="bold") +
      coord_map("polyconic") +
      scale_fill_gradient2(low = '#FFFFFF', high = '#006D2C')+
      theme_void()+
      labs(title = '各省获奖数量',
           caption = sprintf('统计区间：%d-%d年， 统计奖项：%s',input$inp_22[1],input$inp_22[2],trans(input$inp_21)))+
      theme(plot.title=element_text(size = 25, hjust = 0.5),
            plot.caption=element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  
  ##### 第三部分 #####
  output$plot_31 <- renderPlot({
    # 图3-1，获奖最多培养单位
    ggplot(create_data31(), aes(reorder(affiliation, Freq), Freq, label = Freq))+
      geom_bar(stat = 'identity', fill = '#57ADF3', color = 'white')+
      geom_text(size = 5, stat = 'identity', hjust = 1, vjust = 0.5)+
      scale_y_continuous(expand = expand_scale(mult = c(0,0.02)))+
      labs(x = '院校名称', y = '获奖数量',
           title = sprintf('连续%d-%d次获奖数量最多的前%d个培养单位', input$inp_31[1], input$inp_31[2], input$inp_33),
           caption = sprintf('统计区间：%d-%d年， 统计次数：%d-%d', input$inp_32[1], input$inp_32[2], input$inp_31[1], input$inp_31[2]))+
      coord_flip()+
      theme_light()+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 25, hjust = 0.5),
            plot.caption = element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  output$plot_32 <- renderPlot({
    # 图3-2，连续获奖数量分布
    data_32 <- create_data32()
    ggplot()+
      geom_polygon(data = data_32$y1, aes(long, lat, group = group, fill = Count), colour="grey50")+
      geom_text_repel(data = data_32$y2, aes(x = lon, y = lat, label = paste(province, Count, sep = ':')), size=5,
                      family = "myFont", fontface="bold") +
      coord_map("polyconic") +
      scale_fill_gradient2(low = '#FFFFFF', high = '#006D2C')+
      theme_void()+
      labs(title = sprintf('各省连续%d-%d次获奖数量', input$inp_31[1], input$inp_31[2]),
           caption = sprintf('统计区间：%d-%d年， 统计次数：%d-%d',input$inp_32[1], input$inp_32[2], input$inp_31[1], input$inp_31[2]))+
      theme(plot.title=element_text(size = 25, hjust = 0.5),
            plot.caption=element_text(size = 12), 
            text = element_text(family = 'myFont'))
  })
  
  ##### 第四部分 #####
  output$table_41 <- renderDataTable({ 
    data_41 <- create_data4()
    datatable(data_41)
  })
}