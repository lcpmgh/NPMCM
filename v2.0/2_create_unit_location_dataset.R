# 本节程序内容：
#   1. 提取网络上，获奖单位地理位置信息
#   2. 提取获奖数据中，培养单位名称
#   3. 结合网络数据、获奖数据和本地保存的培养单位地理位置数据库（之前生成的1、2的总和）,生成新的位置数据库
#   4. 对新生成的位置数据库，进行培养单位名称标准化，为create_total_data.R中刚转换为标准名字的数据，添加位置信息
#   5. 保存生成的培养单位位置信息数据库到文件（unit_location_dataset.csv），手工补齐剩余未知项，供后续使用
#
# 程序版本：v1.0: 2018-09-15
#           v2.0: 2020-03-07

##### 1. 加载包 #####
library("magrittr")
library("RCurl")
library("XML")
library("rvest")
library("stringr")
library("dplyr")
library("data.table")

##### 2. 更改路径，设置环境 #####
rm(list = ls())
gc()
options(stringsAsFactors = F)
file_dir <- function(name = NULL){paste0('D:/#R/GMCM/v2.0/data-GB2312/', name)}

##### 3. 创建函数 #####
# 创建函数-网络上的，培养单位名称爬虫
training_unit_info <- function(){
  res <- data.table()
  n    <- 1
  ourl <- 'https://yz.chsi.com.cn'
  page <- paste0(ourl, "/sch/search.do?start=0")
  while(page!=paste0(ourl,'#')){
    url <- page
    headers <- c("Referer" = url,
                 "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36")
    debugInfo <- debugGatherer()
    handle    <- getCurlHandle(debugfunction = debugInfo$update, followlocation = TRUE, cookiefile = "", verbose = TRUE)
    content   <- getURL(url, .opts = list(httpheader = headers), .encoding = "utf-8", curl = handle)
    temp_content <- read_html(content) 
    info_table   <- html_table(temp_content) %>% '[['(1) %>% .[,1:2]
    res <- rbind(res, info_table)
    next_page <-  html_nodes(temp_content, "div[class='pager-box clearfix'] li a") %>% 
      html_attr('href') %>% 
      tail(1) %>% 
      paste0(ourl,.)
    message(sprintf('第【%d】页收集完毕',n))
    page <- next_page
    n <- n+1
    Sys.sleep(runif(1, 1, 3))
  }
  names(res) <- c('affiliation', 'location')
  message(sprintf('所有数据爬取完毕，共【%d】条',nrow(res)))
  # 这部分数据不能动，全部为utf-8，要先写入更改编码，若修改，会出现一半gbk一半utf-8的情况
  return(res)
}
# 创建函数-获奖数据中的，培养单位名称提取
unit_extract <- function(){
  total_data <- fread(file_dir('total_data.csv'))
  c_member <- total_data[,c(11,4)] %>% set_colnames(c('team_id','affiliation')) %>% .[,sig:='c']
  f_member <- total_data[,c(11,6)] %>% set_colnames(c('team_id','affiliation')) %>% .[,sig:='f']
  s_member <- total_data[,c(11,8)] %>% set_colnames(c('team_id','affiliation')) %>% .[,sig:='s']
  check_data <- rbind(c_member,f_member,s_member)
  unit_location_1 <- data.table(affiliation = unique(unlist(check_data$affiliation))) %>% 
    .[affiliation!='', ] %>% 
    .[, c('country', 'province'):=list(NA_character_, NA_character_)]
  return(unit_location_1)
}
# 创建函数-整合网络数据、获奖数据和本地数据库
join_unit <- function(unit_location_0, unit_location_1){
  location_convert <- fread(file_dir('location_convert.csv'))
  unit_location_0 <- unit_location_0[location_convert, on = 'location', nomatch=0] %>% 
    .[,c('affiliation','country','province')]         #这里先给网络数据添加信息，由于乱码不能放在爬虫函数中的
  unit_location_dataset <- fread(file_dir('unit_location_dataset.csv')) %>% 
    rbind(unit_location_0) %>%      #注意这里rbind顺序
    rbind(unit_location_1) %>%      #注意这里rbind顺序，要把location有NA的放在最后
    .[!duplicated(affiliation),]  
  return(unit_location_dataset)
}
# 创建函数-培养单位信息中的培养单位名称标准化
standardize_2 <- function(unit_location_dataset, unit_name_identity){
  # 用原始名字中的国家省份信息，赋值给新整理但数据缺失的、标准名字中的国家和省份
  for(i in 1:nrow(unit_name_identity)){
    org_name <- unit_name_identity[i, 1]
    sta_name <- unit_name_identity[i, 2]
    org_country <- unit_location_dataset[affiliation==org_name, country] %>% '['(1)
    org_province <- unit_location_dataset[affiliation==org_name, province] %>% '['(1)
    if(is.na(org_country)){message(sprintf('---未找到【%s】', org_name)); next()}
    unit_location_dataset[affiliation==sta_name, c('country', 'province'):=list(org_country, org_province) ]
    message(sprintf('已将【%s】信息补全', sta_name))
  }
  unit_location_dataset <- unit_location_dataset[!affiliation %in% unit_name_identity$original,]
  return(unit_location_dataset)
}

##### 4. 执行-收集网络上培养单位名称、整合本地培养单位名称、合并到数据库、数据库培养单位名称标准化 #####
unit_location_0 <- training_unit_info()                         #爬虫网络数据
fwrite(unit_location_0, file_dir('unit_location_0.csv'))        #写入爬虫结果，之后在外部改编码（否则utf-8和gb2312冲突）
unit_location_0 <- fread(file_dir('unit_location_0.csv'))       #读取网络数据
unit_location_1 <- unit_extract()                               #整理本地数据
unit_location_dataset <- join_unit(unit_location_0, unit_location_1)      #合并网络与本地数据（包含获奖数据和位置数据库）
unit_name_identity <- fread(file_dir('unit_name_identity_dataset.csv'))   #读取培养单位名称标准化替换数据 
unit_location_dataset <- standardize_2(unit_location_dataset, unit_name_identity)   #进行培养单位信息标准化

##### 5. 更新总数据结果 #####
fwrite(unit_location_dataset, file_dir('unit_location_dataset.csv'))

##### 6. 人工补齐数据 #####
# 之后将unit_location_dataset中没有位置信息的，手动进行添加并原格式保存