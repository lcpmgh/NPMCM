library(shiny)
library(data.table)
library(magrittr)
library(ECharts2Shiny)
library(shinyWidgets)
library(ggplot2)
library(rgdal)
library(ggrepel)
library(DT)
library(showtext)


ui <- fluidPage(
  loadEChartsLibrary(),
  loadEChartsTheme("shine"),
  tabsetPanel(
    type = 'pills',
    tabPanel('By team',
             h3("获奖数量年际变化", align = "center"),
             tags$div(id = "plot_11", style = "width:100%;height:300px;"),
             deliverChart(div_id = "plot_11"),
             tags$hr(),
             h3("获奖题型构成年际变化", align = "center"),
             tags$div(id="plot_12", style = "width:100%;height:300px;"),
             deliverChart(div_id = "plot_12"),
             tags$div(style = 'width:100%;height:30px'),
             tags$hr(),
             tags$footer("---Designed by pmgh---", align = 'center')),
    tabPanel('By member',
             dropdownButton(tags$h4(tags$b("参数选项")), 
                            circle = T, 
                            status = "success", 
                            icon = icon("gear"), 
                            width = "380px",
                            tooltip = tooltipOptions(title = "参数选项"),
                            awesomeCheckboxGroup(inputId = "inp_21", label = "获奖类型",
                                                 choices = c("一等奖", "二等奖", "三等奖"),
                                                 selected = c("一等奖", "二等奖", "三等奖"), inline = T),
                            sliderTextInput(inputId = "inp_22", label = "年份区间", choices = 2004:2019, selected = c(2004,2019)),
                            sliderTextInput(inputId = "inp_23", label = "显示数量", choices = 1:25, selected = 10)),
             plotOutput("plot_21", width = '100%', height = '600px'),
             tags$hr(),
             plotOutput("plot_22", width = '100%', height = '650px'),
             tags$hr(),
             tags$footer("---Designed by pmgh---", align = 'center')),
    tabPanel('Award Series',
             dropdownButton(tags$h4(tags$b("参数选项")), circle = T, status = "success", icon = icon("gear"), width = "300px",
                            tooltip = tooltipOptions(title = "参数选项"),
                            sliderTextInput(inputId = "inp_31", label = "连续获奖次数",
                                            choices = seq(1:6), selected = c(1, 6)),
                            sliderTextInput(inputId = "inp_32", label = "年份区间", choices = 2004:2019, selected = c(2004, 2019)),
                            sliderTextInput(inputId = "inp_33", label = "显示数量", choices = 1:25, selected = 10)),
             plotOutput("plot_31", width = '100%', height = '500px'),
             tags$hr(),
             plotOutput("plot_32", width = '100%', height = '650px'),
             tags$hr(),
             tags$footer("---Designed by pmgh---", align = 'center')),
    tabPanel('Award database',
             h3("获奖队伍查询系统", align = "center"),
             sidebarLayout(
               sidebarPanel(width = 3,
                            tags$hr(),
                            pickerInput(inputId = "inp_41",
                                        label = "选择年份", 
                                        choices = 2004:2019,
                                        options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                                        multiple = TRUE),
                            checkboxGroupButtons(
                              inputId = "inp_42",
                              label = "选择奖项",
                              choices = c("一等奖", "二等奖", "三等奖"),
                              status = "primary",
                              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))),
                            checkboxGroupButtons(
                              inputId = "inp_43",
                              label = "选择题型",
                              choices = c("A","B","C","D","E","F","N"),
                              checkIcon = list( yes = tags$i(class = "fa fa-check-square",style = "color: steelblue"),
                                                no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
                            tags$hr(),
                            h6("由于输入和输出的编码转换存在问题，姓名和校名的查询不能在此实现，请通过右侧search功能查询QAQ")
               ),
               mainPanel(dataTableOutput("table_41"))
             ),
             tags$hr(),
             tags$footer("---Designed by pmgh---", align = 'center')
    )
  )
)
