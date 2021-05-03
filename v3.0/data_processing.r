# 本程序内容：
#     整合获奖数据文件为一个数据，并进行培养单位名称标准化、生成培养单位信息、生成成员获奖数据，为后续可视化做准备。
# 
# 程序版本：
#     v1.0：2021-04-26


##### 1. 加载包 #####
# 1.1 数据处理部分
library(magrittr)
library(data.table)
library(stringr)
# 1.2 网络爬虫部分
library(RCurl)
library(XML)
library(rvest)


##### 2. 配置环境 #####
rm(list = ls())
gc()
options(stringsAsFactors = F)                                                                 #指定字符读取方式和默认编码
setwd(choose.dir(default = 'D:/#R/NPMCM'))                                                    #将临时路径指定到NPMCM文件夹
raw_data_dir <- function(name = NULL){paste0(getwd(), '/awardlist/', name)}                   #原始文件路径，用getwd只为message的清楚显示
data_dir <- function(name = NULL){paste0(getwd(), '/v3.0/shiny_app/data/', name)}             #数据文件路径
my_read_csv <- function(file_name, raw_data = F, ...){
  if(isTRUE(raw_data)){
    file_name <- raw_data_dir(file_name)
  } else{
    file_name <- data_dir(file_name)
  }
  data <- fread(file_name, encoding = 'UTF-8', ...)
  message(sprintf("已读取UTF-8文件【%s】", file_name))
  return(data)
}                                                                                             #创建闭包读取函数（强制读为UTF-8）
my_write_csv <- function(data, file_name){
  file_name <- data_dir(file_name)
  write.csv(data, file_name, row.names = F, quote = F, fileEncoding = 'UTF-8')
  message(sprintf("已写入UTF-8文件【%s】", file_name))
}                                                                                             #创建闭包写入函数（强制写为UTF-8）


##### 3. 创建数据处理函数 #####
# 3.1 整合分年获奖数据，用于后续数据处理
data_collection <- function(){
  total_data <- data.table()
  files_name <- list.files(raw_data_dir()) %>% str_extract("\\d{4,4}.csv") %>% na.omit()      #识别原始数据文件夹中有多少年的获奖数据
  for(file_name in files_name){
    t <- my_read_csv(file_name, raw_data = T, integer64 = 'character', select = 1:9)                        #转换队号列数据类型，并排除2020年备注列
    t[, Year:=str_extract(file_name, "\\d{4,4}")]                                             #添加year
    total_data <- rbind(total_data, t)
  }
  total_data <- dplyr::rename(total_data, 
                              Q_type = '题目类型',      T_numb = '队号',               
                              C_name = '队长姓名',      C_unit = '队长所在学校',
                              F_name = '第一队友姓名',  F_unit = '第一队友所在学校', 
                              S_name = '第二队友姓名',  S_unit = '第二队友所在学校', 
                              A_type = '所获奖项') %>% 
    .[, T_id:=seq_along(A_type)] %>% 
    .[!(A_type %in% c('一等奖', '二等奖', '三等奖')), A_type:='成功参与奖']                   #变量重命名、生成唯一的队伍id、归一奖项名
  message(sprintf('已整合如上【%d份】获奖名单文件。', length(files_name)))
  return(total_data)
}
# 3.2 提取获奖数据中的培养单位名称，用于单位名识别、归一、定位
unit_collection <- function(total_data){
  total_unit <- melt(total_data, id.vars = 11, measure.vars = c(4,6,8), variable.name = 'Sig', value.name = 'Unit') %>% 
    .[,Sig:=as.character(Sig)]
  return(total_unit)
}
# 3.3 获奖数据中的培养单位名称标准化
unit_standardization <- function(total_data){
  unit_name_convert <- my_read_csv('0-unit_name_convert.csv')                                 #读取培养单位名称标准化替换数据
  total_unit <- unit_collection(total_data)
  for(i in 1:nrow(unit_name_convert)){
    o_name <- unit_name_convert[i,1]
    s_name <- unit_name_convert[i,2]
    check <- total_unit[Unit==o_name,]
    if(nrow(check)==0){
      message(sprintf('-----未找到【%s】', o_name))
      next()
    }
    for(j in 1:nrow(check)){
      tcheck_T_id <- check[[j,1]]
      tcheck_Sig <- check[[j,2]] %>% as.character()
      total_data[T_id==tcheck_T_id, I(tcheck_Sig):=s_name]
    }
    message(sprintf('已将【%s】替换为【%s】', o_name, s_name))
  }
  return(total_data)
}
# 3.4 爬虫获取培养单位信息
unit_info_crawler <- function(){
  res <- data.table()
  n    <- 1
  ourl <- 'https://yz.chsi.com.cn'
  page <- paste0(ourl, "/sch/search.do?start=0")
  message(">>>正在进行网络爬虫...")
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
  names(res) <- c('Unit', 'Location')
  unit_location_convert <- my_read_csv('0-unit_location_convert.csv')
  unit_info_internet <- res[unit_location_convert, on='Location', nomatch=0] %>% .[,-2]                  #将培养单位位置转换为培养单位信息       
  message(sprintf('>>>爬虫结束，共收集到【%d】条数据',nrow(res)))
  return(unit_info_internet)
}
# 3.5 生成培养单位信息数据库
unit_info_genrating <- function(crawler = F){
  if(isTRUE(crawler)){
    unit_info_internet <- unit_info_crawler()                                                            #可选，网上培养单位信息爬虫
    my_write_csv(unit_info_internet, '1-unit_info_internet.csv')
    
  } else {
    unit_info_internet <- my_read_csv('1-unit_info_internet.csv')                                        #可选，读取培养单位信息爬虫结果
  }
  unit_info_dataset <- my_read_csv('3-unit_info_dataset.csv') %>%
    rbind(unit_info_internet) %>% 
    .[!duplicated(Unit), ]                                                                               #读取培养单位信息库，合并爬虫数据
  data_unit <- unit_collection(total_data) %>% .[,3] %>% unique()                                        #提取获奖名单中的培养单位
  unit_info_dataset <- dplyr::full_join(unit_info_dataset, data_unit, by='Unit') %>% setorder(Province)  #与获奖名单中单位的合并
  return(unit_info_dataset)
}
# 3.6 区分个体并计算成员连续获奖次数
series_finding <- function(year, group){
  dy <- diff(year)
  if(any(dy<0)) stop('没排序啊!')
  ldy <- length(dy)
  s <- integer(ldy+1)                            #统计重复次数
  g <- character(ldy+1)                          #统计分组信息
  if(ldy==0){                                    #不必要，但是除去会使效率降低15%以上
    s <- 1
    g <- paste(group, '1', sep='-')
    return(list(sig = s, gro = g))
  } else{
    sig <- c(0, dy, 0)
    sig[sig>1] <- 0
    w <- which(sig==0)                           #这里的w长度至少大于2
    n <- 1
    for(i in 2:length(w)){
      lo <- w[i-1]:(w[i]-1)
      le <- seq_along(lo)
      s[lo] <- le
      g[lo] <- paste(group[1], n, sep = '-')    #将所有断开的连续视为不同个体，更符合大多数情况，无奈之举
      n <- n+1
    }
    return(list(sig = s, gro = g))
  }
}
# 3.7 生成成员获奖数据
member_data_generating <- function(total_data){
  unit_info_dataset <- my_read_csv('3-unit_info_dataset.csv')
  member_data <- total_data %>% 
    .[A_type %in% c('一等奖','二等奖','三等奖'),] %>% 
    melt(id=c(11,1,10,9), measure.vars=list(Name=c(3,5,7), Unit=c(4,6,8))) %>%
    .[Name!='',] %>% 
    .[, c(1,3,4,6,7)]%>% 
    .[, M_id:=seq_along(T_id)] %>% 
    .[, Year:=as.numeric(Year)] %>% 
    .[order(Year)] %>% 
    .[, c('Series','Group'):=list(series_finding(Year, .GRP)$sig, series_finding(Year, .GRP)$gro), by=c('Name','Unit')] %>% 
    .[, M_unique:=.GRP, by='Group'] %>% 
    unit_info_dataset[., on='Unit'] %>% 
    .[,c(4,8,11,9,5,6,7,1,2,3)]
  return(member_data)
}


##### 4. 执行数据处理-生成总数据、将其中的培养单位名称标准化 #####
# 4.1 将年份获奖数据整合
total_data <- data_collection()                                      #整合获奖数据
# 4.2 培养单位名称归一化
total_data <- unit_standardization(total_data)                       #单位名称标准化
# 4.3 生成培养单位信息（初步，后续需人工查验补齐）
unit_info_dataset <- unit_info_genrating(crawler = T)
# 4.4 生成获奖名单（处理后的）中的单位名称集（唯一化）
total_unit_unique <- unit_collection(total_data) %>% .[!duplicated(Unit)] %>% .[order(Unit)]
# 4.5 生成成员获奖数据
member_data <- member_data_generating(total_data)


##### 5. 保存数据-保存合并的获奖名单和培养单位信息数据库 #####
# 5.1 保存合并获奖名单数据
my_write_csv(total_data, '2-total_data.csv')
# 5.2 保存培养单位信息数据库
my_write_csv(unit_info_dataset, '3-unit_info_dataset.csv')
# 5.3 保存单位名称集
my_write_csv(total_unit_unique, '4-total_unit_unique.csv')
# 5.4 保存成员获奖数据
my_write_csv(member_data, '5-member_data.csv')


