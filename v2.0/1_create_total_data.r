# 本节程序内容：
#   1. 将原始数据整合到一起
#   2. 将培养单位名称信息进行整理，尽可能将名称统一
#   3. 保存经处理后的合并数据到文件夹中（total_data.csv）供后续使用
# 
# 程序版本：
#     v2.0：2020-03-08

##### 1. 加载包 #####
library(magrittr)
library(data.table)
library(stringr)

##### 2. 更改路径，设置环境 #####
rm(list = ls())
gc()
file_dir <- function(name = NULL){paste0('D:/#R/GMCM/v2.0/data-GB2312/', name)}
options(stringsAsFactors = F)

##### 3. 创建函数 #####
# 创建函数-读取全部数据
collect_data <- function() {
  total_data <- data.table()
  files_name <- list.files(file_dir()) %>% str_extract("\\d{4,4}.csv") %>% na.omit()
  for(file_name in files_name){
    t <- fread(file_dir(file_name))
    t[, year:=str_extract(file_name, "\\d{4,4}")] #添加year
    t[, '队号':=as.character(get('队号'))]        #2018年之后出现了字符型队号
    total_data <- rbind(total_data, t)
  }
  total_data <- dplyr::rename(total_data, 
                              question_type = '题目类型',
                              team_no       = '队号',
                              c_name        = '队长姓名',
                              c_affiliation = '队长所在学校',
                              f_name        = '第一队友姓名',
                              f_affiliation = '第一队友所在学校',
                              s_name        = '第二队友姓名',
                              s_affiliation = '第二队友所在学校',
                              award_type    = '所获奖项')
  total_data$team_id <- 1:nrow(total_data)                        #用类似total_data[, team_id:=seq_along(award_type)]的方法，会出warning，说复制了影响性能
  return(total_data)
}
# 创建函数-获奖数据中的培养单位名称标准化
standardize_1 <- function(total_data, unit_name_identity){
  unit_name_identity <- unit_name_identity %>% .[, replace:=TRUE] %>% .[!duplicated(original),]
  c_member <- total_data[,c(11,4)] %>% set_colnames(c('team_id','affiliation')) %>% .[,sig:='c']
  f_member <- total_data[,c(11,6)] %>% set_colnames(c('team_id','affiliation')) %>% .[,sig:='f']
  s_member <- total_data[,c(11,8)] %>% set_colnames(c('team_id','affiliation')) %>% .[,sig:='s']
  check_data <- rbind(c_member,f_member,s_member)
  for(i in 1:nrow(unit_name_identity)){
    org_name <- unit_name_identity[i, 1]
    sta_name <- unit_name_identity[i, 2]
    tar <- check_data[affiliation==org_name,]
    nl <- nrow(tar)
    if(nl==0) {
      message(sprintf('---未找到【%s】', org_name))
      unit_name_identity[i, replace:=FALSE]
      next()
    }
    for(j in 1:nl){
      ttid <- tar[j, team_id]
      colname <- paste0(tar[j, sig], '_', 'affiliation')
      total_data[team_id==ttid, I(colname):=sta_name]
      message(sprintf('已将【%s】替换为【%s】', org_name, sta_name))
    }
  }
  return(list(x1=total_data, x2=unit_name_identity))
}

##### 4. 执行-生成总数据、将其中的培养单位名称标准化 #####
total_data <- collect_data()                                              #生成或读取全部数据（二选一，第一次生成时选择）
total_data <- fread(file_dir('total_data.csv'))                           #生成或读取全部数据（二选一，无获奖数据更新时选择）
unit_name_identity <- fread(file_dir('unit_name_identity_dataset.csv'))   #读取培养单位名称标准化替换数据 
result <- standardize_1(total_data, unit_name_identity)                   #进行培养单位信息标准化（全部未找到标准化完成）
total_data         <- result$x1        #提取结果中的标准化后的总数据
standardize_result <- result$x2        #提取结果中的替换信息（查看用）

##### 5. 更新总数据结果 #####
fwrite(total_data, file_dir('total_data.csv'))
