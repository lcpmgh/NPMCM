server <- function(input, output){
  ##### 0. Preparation #####
  # Chinese font
  font_add("myFont", './data/0-typeface.ttc')    
  showtext_auto()
  # functions
  my_read_csv <- function(file_name, ...){
    # For data reading
    data <- fread(paste0('./data/', file_name), encoding = 'UTF-8', ...)
    # data <- fread(paste0('./shiny_app/data/', file_name), encoding = 'UTF-8', ...)
    return(data)
  }
  trans <- function(x){
    # For legends sorting
    y1 <- y2 <- y3 <- NA_character_
    if('一等奖' %in% x) y1 <- '一等奖'
    if('二等奖' %in% x) y2 <- '二等奖'
    if('三等奖' %in% x) y3 <- '三等奖'
    y <- paste(na.omit(c(y1, y2, y3)), collapse = '、')
    return(y)
  }
  # data
  data_team   <- my_read_csv("2-total_data.csv") %>% .[, A_type:=factor(A_type, levels = c('一等奖','二等奖','三等奖','成功参与奖'), order = T)]
  data_member <- my_read_csv("5-member_data.csv") %>% .[, A_type:=factor(A_type, levels = c('一等奖','二等奖','三等奖'), order = T)]
  
  ##### 1. Page1 #####
  output$ui_p11 <- renderUI({
    tagList(
      pickerInput(inputId = "p1_input1",
                  label = "选择年份",
                  choices = unique(data_team$Year),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                  multiple = TRUE),
      checkboxGroupButtons(inputId = "p1_input2",
                           label = "选择奖项",
                           choices = c("一等奖", "二等奖", "三等奖", "成功参与奖"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))),
      checkboxGroupButtons(inputId = "p1_input3",
                           label = "选择题型",
                           choices = c("A", "B", "C", "D", "E", "F", "N"),
                           checkIcon = list( yes = tags$i(class = "fa fa-check-square",style = "color: steelblue"),
                                             no = tags$i(class = "fa fa-square-o", style = "color: steelblue")))
    )
  })
  output$ui_p12 <- renderUI({
    tagList(
      h3("获奖数据查询", align = "center"),
      reactableOutput("p1_table")
    )
  })
  output$p1_table <- renderReactable({
    if(length(input$p1_input1) == 0) sYear <- unique(data_team$Year) else sYear <- input$p1_input1
    if(length(input$p1_input2) == 0) sA_type <- unique(data_team$A_type) else sA_type <- input$p1_input2
    if(length(input$p1_input3) == 0) sQ_type <- unique(data_team$Q_type) else sQ_type <- input$p1_input3
    p1_data <- data_team %>% .[Year %in% sYear & A_type %in% sA_type & Q_type %in% sQ_type, 1:10] %>% 
      set_colnames(c("题目类型", "队伍编号", "队长姓名", "队长培养单位", "第一队员姓名", "第一队员培养单位", "第二队员姓名", "第二队员培养单位", "所获奖项", "参赛年份"))
    reactable(p1_data,
              defaultPageSize = 20,
              searchable  = T,
              showPageSizeOptions = T,
              highlight = T,
              striped = T,
              compact = T,
              resizable = T,
              height = "700px",
              fullWidth = T)
  })
  
  ##### 2. Page2 #####
  output$ui_p2 <- renderUI({
    tagList(
      h3("获奖率（单位%）", align = "center"),
      tags$div(id = "p2_plot1", style = "width:100%;height:300px;"),
      deliverChart(div_id = "p2_plot1"),
      tags$hr(),
      h3("获奖数量", align = "center"),
      tags$div(id = "p2_plot2", style = "width:100%;height:300px;"),
      deliverChart(div_id = "p2_plot2"),
      tags$hr(),
      h3("获奖数量", align = "center"),
      loadEChartsTheme("shine"),
      tags$div(id = "p2_plot3", style = "width:100%;height:300px;"),
      deliverChart(div_id = "p2_plot3"),
      tags$hr(),
      h3("获奖构成比例（单位%）", align = "center"), 
      loadEChartsTheme("infographic"),
      tags$div(id="p2_plot4", style = "width:100%;height:300px;"),
      deliverChart(div_id = "p2_plot4"),
      tags$hr(),
      h3("获奖构成比例（单位%）", align = "center"), 
      loadEChartsTheme("infographic"),
      tags$div(id="p2_plot5", style = "width:100%;height:300px;"),
      deliverChart(div_id = "p2_plot5")
    )
  })
  p2_data1 <- data_team %>%
    dcast(Year~A_type, value.var = 'Year', fun = length) %>%
    .[, s:=apply(.SD, 1, sum), .SDcols=2:5] %>%
    .[, round(.SD/s*100,2), .SDcol=2:5, by='Year'] %>%
    .[, s:=100-.SD, .SDcols = 5] %>%
    .[s==100, c('一等奖', '二等奖', '三等奖', 's'):=.(NA, NA, NA, NA)] %>%
    as.data.frame() %>%
    set_rownames(., .[,1]) %>%
    dplyr::rename('综合获奖率' = s) %>%
    .[, c(2, 3, 4, 6)]
  renderLineChart(div_id = "p2_plot1",  data = p2_data1, axis.x.name = '年份', axis.y.name = '百分数(%)', theme = 'shine')
  p2_data2 <- data_team %>%
    .[A_type!='成功参与奖', ] %>%
    dcast(Year~A_type, value.var = 'Year', fun = length) %>%
    .[, I('总获奖'):=sum(get('一等奖'), get('二等奖'), get('三等奖')), by=.(Year)] %>%
    as.data.frame() %>%
    set_rownames(.,.[,1]) %>%
    .[, -1]
  renderLineChart(div_id = "p2_plot2", data = p2_data2, axis.x.name = '年份', axis.y.name = '数量', theme = 'shine')
  p2_data3 <- data_team %>%
    .[A_type!='成功参与奖', ] %>%
    dcast(Year~Q_type, value.var = 'Year', fun = length) %>%
    .[, I('总获奖'):=sum(.SD), by = "Year", .SDcols = 2:8] %>% 
    as.data.frame() %>%
    set_rownames(., .[,1]) %>%
    .[, -1]
  renderLineChart(div_id = "p2_plot3", data = p2_data3, axis.x.name = '年份', axis.y.name = '数量', theme='shine')
  p2_data4 <- p2_data2[, 1:3] %>%
    divide_by(apply(., 1, sum)) %>%
    multiply_by(100) %>%
    round(2)
  renderBarChart(div_id = "p2_plot4", data = p2_data4, axis.x.name = '年份', axis.y.name = '百分数(%)', theme='infographic', direction = "vertical", grid_left = "10%", stack_plot = T)
  p2_data5 <- p2_data3[, 1:7] %>%
    divide_by(apply(., 1, sum)) %>%
    multiply_by(100) %>%
    round(2)
  renderBarChart(div_id = "p2_plot5", data = p2_data5, axis.x.name = '年份', axis.y.name = '百分数(%)', theme='infographic', direction = "vertical", grid_left = "10%", stack_plot = T)
  
  ##### 3. Page3 #####
  output$ui_p31 <- renderUI({
    dropdownButton(tags$h4(tags$b("参数选项")), 
                   circle = T, 
                   status = "success", 
                   icon = icon("cog"),
                   width = "380px",
                   tooltip = tooltipOptions(title = "参数选项"),
                   awesomeCheckboxGroup(inputId = "p3_input1", 
                                        label = "获奖类型",
                                        choices = c("一等奖", "二等奖", "三等奖"),
                                        selected = c("一等奖", "二等奖", "三等奖"), 
                                        inline = T),
                   sliderTextInput(inputId = "p3_input2", 
                                   label = "年份区间", 
                                   choices = unique(data_team$Year), 
                                   selected = c(min(unique(data_team$Year)), max(unique(data_team$Year)))),
                   sliderTextInput(inputId = "p3_input3", 
                                   label = "显示数量", 
                                   choices = 1:25, 
                                   selected = 10))
  })
  output$ui_p32 <- renderUI({
    tagList(
      h3(sprintf('获奖人次最多的%d个培养单位', input$p3_input3), align = "center"), 
      plotOutput(outputId = "p3_plot1", width = '100%', height = '600px'),
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s）', input$p3_input2[1], input$p3_input2[2], trans(input$p3_input1)), align = "right"),
      tags$hr(),
      h3('各省获奖人次', align = "center"), 
      REmapOutput(outputId = "p3_plot2"),
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s）', input$p3_input2[1], input$p3_input2[2], trans(input$p3_input1)), align = "right")
    )
  })
  output$p3_plot1 <- renderPlot({
    p3_data1 <- data_member %>%
      .[, c('Unit', 'Year', 'A_type')] %>%
      .[A_type %in% input$p3_input1 & Year>=input$p3_input2[1] & Year<=input$p3_input2[2], ] %>%
      .[, .(Freq = length(Year)), by = .(Unit)] %>%
      setorder(-Freq) %>%
      .[1:min(nrow(.), input$p3_input3), ]
    ggplot(p3_data1, aes(reorder(Unit, Freq), Freq, label = Freq))+
      geom_bar(stat = 'identity', fill = '#57ADF3', color = 'white')+
      geom_text(size = 9, stat = 'identity', hjust = 0, vjust = 0.5)+
      scale_y_continuous(expand = expansion(mult = c(0,0.3)))+
      labs(x = "", y = "")+
      coord_flip()+
      theme_light()+
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            plot.caption = element_text(size = 15), 
            text = element_text(family = 'myFont'))
  })
  output$p3_plot2 <- renderREmap({
    tdata <- data.table(Province = c("北京","天津","上海","重庆","河北","山西","辽宁","吉林","黑龙江","江苏","浙江","安徽","福建","江西","山东","河南",
                                     "湖北","湖南","广东","海南","四川","贵州","云南","陕西","甘肃","青海","台湾","内蒙古","广西","西藏","宁夏","新疆", 
                                     "香港","澳门" ))
    p3_data2 <- data_member %>% 
      .[Country == "中国" & A_type %in% input$p3_input1 & Year>=input$p3_input2[1] & Year<=input$p3_input2[2], 10] %>%
      table() %>% 
      as.data.table() %>% 
      set_colnames(c("Province", "Freq")) %>% 
      .[tdata, on = "Province"] %>% 
      .[is.na(Freq), Freq:=0]
    remapC(data = p3_data2)
  })
  
  ##### 4. Page4 #####
  output$ui_p41 <- renderUI({
    dropdownButton(tags$h4(tags$b("参数选项")), 
                   circle = T, 
                   status = "success", 
                   icon = icon("cog"), 
                   width = "300px",
                   tooltip = tooltipOptions(title = "参数选项"),
                   awesomeCheckboxGroup(inputId = "p4_input1", 
                                        label = "获奖类型",
                                        choices = c("一等奖", "二等奖", "三等奖"),
                                        selected = c("一等奖", "二等奖", "三等奖"), 
                                        inline = T),
                   sliderTextInput(inputId = "p4_input2", 
                                   label = "连续获奖次数",
                                   choices = 1:max(data_member$Series), 
                                   selected = c(1, max(data_member$Series))),
                   sliderTextInput(inputId = "p4_input3", 
                                   label = "年份区间", 
                                   choices = unique(data_team$Year), 
                                   selected = c(min(unique(data_team$Year)), max(unique(data_team$Year)))),
                   sliderTextInput(inputId = "p4_input4", 
                                   label = "显示数量",
                                   choices = 1:25, 
                                   selected = 10))
  })
  output$ui_p42 <- renderUI({
    tagList(
      h3(sprintf('连续%d-%d次获奖人数最多的%d个培养单位', input$p4_input2[1], input$p4_input2[2], input$p4_input4), align = "center"), 
      plotOutput(outputId = "p4_plot1", width = '100%', height = '600px'),
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s）', input$p4_input3[1], input$p4_input3[2], trans(input$p4_input1)), align = "right"),
      tags$hr(),
      h3(sprintf('各省连续%d-%d次获奖人数', input$p4_input2[1], input$p4_input2[2]), align = "center"), 
      REmapOutput(outputId = "p4_plot2"),
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s）', input$p4_input3[1], input$p4_input3[2], trans(input$p4_input1)), align = "right")
    )
  })
  output$p4_plot1 <- renderPlot({
    p4_data1 <- data_member %>% 
      .[A_type %in% input$p4_input1 & Series>=input$p4_input2[1] & Series<=input$p4_input2[2] & Year>=input$p4_input3[1] & Year<=input$p4_input3[2], ] %>%
      .[, .(Freq=length(M_unique)), by = .(Unit)] %>% 
      setorder(-Freq) %>% 
      .[!is.na(Unit), ] %>% 
      .[1:min(nrow(.), input$p4_input4), ] %>% 
      na.omit()
    if(nrow(p4_data1) == 0){
      ggplot(data = NULL, aes(x=1, y=2, label = "无数据")) + geom_text(size = 15) + theme_void()
    } else{
      ggplot(p4_data1, aes(reorder(Unit, Freq), Freq, label = Freq))+
        geom_bar(stat = 'identity', fill = '#57ADF3', color = 'white')+
        geom_text(size = 9, stat = 'identity', hjust = 0, vjust = 0.5)+
        scale_y_continuous(expand = expansion(mult = c(0,0.3)))+
        labs(x = "", y = "")+
        coord_flip()+
        theme_light()+
        theme(axis.text = element_text(size = 18),
              axis.title = element_text(size = 20),
              plot.caption = element_text(size = 15), 
              text = element_text(family = 'myFont'))
    }
  })
  output$p4_plot2 <- renderREmap({
    tdata <- data.table(Province = c("北京","天津","上海","重庆","河北","山西","辽宁","吉林","黑龙江","江苏","浙江","安徽","福建","江西","山东","河南",
                                     "湖北","湖南","广东","海南","四川","贵州","云南","陕西","甘肃","青海","台湾","内蒙古","广西","西藏","宁夏","新疆", 
                                     "香港","澳门" ))
    p4_data2_t <- data_member %>% 
      .[Country == "中国" & A_type %in% input$p4_input1 & Series>=input$p4_input2[1] & Series<=input$p4_input2[2] & Year>=input$p4_input3[1] & Year<=input$p4_input3[2], 10] %>%
      table() 
    if(nrow(p4_data2_t) == 0){
      p4_data2 <- data.frame(tdata, value=0)
    } else{
      p4_data2 <- p4_data2_t %>% 
        as.data.table() %>% 
        set_colnames(c("Province", "Freq")) %>% 
        .[tdata, on = "Province"] %>% 
        .[is.na(Freq), Freq:=0]
    }
    remapC(data = p4_data2)
  })
  
}