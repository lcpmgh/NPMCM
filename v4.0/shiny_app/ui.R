# shiny获奖信息可视化程序
# Version:
#          v2: 2021.05.01
#          v3: 2022.01.29   Switch to REmap package (from rgdal package)

##### 1. load packages #####
library(shiny)
library(showtext)
library(data.table)
library(magrittr)
library(ECharts2Shiny)
library(shinyWidgets)
library(ggplot2)
library(REmap)
library(DT)

##### 2. ui function #####
ui <- fluidPage(
  loadEChartsLibrary(),
  loadEChartsTheme("shine"),
  titlePanel(title = "中国研究生数学建模竞赛获奖信息可视化系统"),
  tabsetPanel(id = "mainpanel",
              type = 'pills',
              tabPanel(title = '获奖名单', sidebarLayout(sidebarPanel(uiOutput("ui_p11")), mainPanel(uiOutput("ui_p12")))),
              tabPanel(title = '按队伍', uiOutput("ui_p2")),
              tabPanel(title = '按个人', uiOutput("ui_p31"), uiOutput("ui_p32")),
              tabPanel(title = '个人连续获奖', uiOutput("ui_p41"), uiOutput("ui_p42"))
              ),
  tags$hr(),
  column(12, align = "center", actionLink(inputId = "github",
                                            label = "Github",
                                            icon = icon("github"),
                                            onclick ="window.open('https://github.com/lcpmgh/NPMCM')")),
  tags$footer("--- Designed by PMGH ---", align = 'center')

)
