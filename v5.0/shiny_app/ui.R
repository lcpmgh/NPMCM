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
  tags$div(align = "center", 
           tags$p("--- Designed by PMGH ---"),
           actionLink(inputId = "", label = "Github", icon = icon("github"), onclick ="window.open('https://github.com/lcpmgh/NPMCM')"),
           tags$p("  ", style = "display:inline;white-space:pre"),
           tags$p("Email: lcpmgh@gmail.com", style="display:inline;white-space:pre"),  
           tags$div(align = "center",
                    tags$a("冀ICP备2022003075号", target="_blank", href="https://beian.miit.gov.cn", style="color:#06c; display:inline;"),
                    tags$p("  ", style = "display:inline;white-space:pre"),
                    tags$img(src="http://www.beian.gov.cn/portal/download"),
                    tags$a("川公网安备51010702002736", target="_blank", href="http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", style="color:#06c; display:inline;")
           )
  )
  

)
