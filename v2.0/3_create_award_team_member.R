# 本节程序内容：
#   1. 生成获奖队伍数据（award_team.csv）
#   2. 设计算法计算成员连续获奖信息（award_member.csv）
#   3. 两个数据作为后续绘图使用
#
# 程序版本与性能：
#   v0.5：2018-01-01 
#         用户  系统  流逝 
#         60.30  0.04 61.50 
#   v1.0：2018-09-19
#         用户  系统  流逝 
#         28.84  0.06 28.97 
#   v2.0：2020-03-07 （电脑升级到了i7的，有硬件升级加持） 
#         用户 系统 流逝 
#         3.23 0.53 3.79 

# 说明：
#   1. 由于用差分替代递归，对比之前第5节处计算的连续次数会因为重复中断，因此连续数据会更为保守
#   2. 成员连续获奖数据中，member_id统计人次，member_unique统计连续个体

##### 1. 加载包 #####
library(magrittr)
library(data.table)
library(stringr)
library(dplyr)

##### 2. 更改路径，设置环境 #####
rm(list = ls())
gc()
file_dir <- function(name = NULL){paste0('D:/#R/GMCM/v2.0/data-GB2312/', name)}
options(stringsAsFactors = F)

##### 3. 创建函数 #####
# 创建函数-生成获奖队伍数据（原有total_data直接筛选获奖队伍）
create_award_team <- function(){
  award_team <-  fread(file_dir('total_data.csv')) %>% .[award_type %in% c('一等奖', '二等奖', '三等奖'), ]
  return(award_team)
}
# 创建函数-用差分法判断连续获奖年数并将连续数据识别为个体（间断的不划分为同一个个体）
find_series <- function(year, group){
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
    w <- which(sig==0)                           #这里的w长度至少为2
    n <- 1
    for(i in 2:length(w)){
      lo <- w[i-1]:(w[i]-1)
      le <- seq_along(lo)
      s[lo] <- le
      g[lo] <- paste(group[1], n, sep = '-')
      n <- n+1
    }
    return(list(sig = s, gro = g))
  }
}
# 创建函数-生成获奖成员数据
create_award_member <- function(award_team){
  # 转换为宽数据
  award_member <- melt(award_team, id = c(11, 1, 10, 9), 
                       measure.vars = list(name = c(3, 5, 7), affiliation = c(4, 6, 8))) %>% .[, c(1, 3, 4, 6, 7)] 
  award_member <- award_member[name!='', ]         #剔除空队员
  award_member$member_id <- 1:nrow(award_member)   #为人次编号
  award_member <- setorder(award_member, year)     #前提1：必须排序
  award_member[, year:=as.numeric(year)]           #前提2：必须数值型
  award_member[, c('series', 'group'):=list(integer(1), character(1))]    #这句话没用，另一台电脑上运行报错说格式有误，但是这行没能解决
  award_member[, c('series', 'group'):=
                 list(find_series(year, .GRP)$sig, find_series(year, .GRP)$gro),
               by=c('name','affiliation')]         #连续获奖判断并分组（同校同名连续获奖的视为同组，否则不同组）（组名字符）
  award_member[, member_unique:=.GRP, by='group']  #再次进行分组，识别成员个体（组名数值）
  unit_location <- fread(file_dir('unit_location_dataset.csv'))
  award_member <- unit_location[award_member, on = 'affiliation']
  award_member <- award_member[, c("team_id", "member_id", "member_unique", "series", "year","award_type",
                                   "name", "affiliation", "country", "province")]
  return(award_member)
}

##### 4. 执行函数，获奖队伍数据和获奖成员数据 #####
award_team <- create_award_team()
award_member <- create_award_member(award_team)

##### 5. 储存数据 ##### 
fwrite(award_team, file_dir('award_team.csv'))
fwrite(award_member, file_dir('award_member.csv'))
