# 数据处理程序
# 本例可视化分为三层：1按队伍、2按成员、3按连续
# 其中前两类只需对原始数据进行处理
# 本程序对原始数据进行处理，生成第三类可视化所需数据
# 修改时间：2018-09-15, v1.0
#           2018-09-19, v2.0


# 加载包
library(magrittr)
library(dplyr)
library(data.table)


# 读取原始数据并修改变量名，添加全局队伍层面的主键：id
# web端数据在github中的同名文件夹、同名文件
award_data <- NULL
for(year in 2004:2017){
  ta <- fread(paste0('./GMCM/winner-info/',year,'.csv'),stringsAsFactors=F,encoding='UTF-8')
  ta$'year'  <- year
  award_data <- rbind(award_data,ta)
}
university_inf <- fread('./GMCM/winner-info/university_info.csv',stringsAsFactors=F,encoding='UTF-8')
award_data <- dplyr::rename(award_data,
                           question_type='题目类型',
                           team_no='队号',
                           captain_name='队长姓名',
                           captain_affiliation='队长所在学校',
                           fteammate_name='第一队友姓名',
                           fteammate_affiliation='第一队友所在学校',
                           steammate_name='第二队友姓名',
                           steammate_affiliation='第二队友所在学校',
                           award_type='所获奖项'
                           )
award_data$id <- 1:nrow(award_data)

# 转变为成员层面数据，并排除无关变量（队伍层面的team_no/question_type）和数据（成功参与奖）
# 添加member_id，原始数据只到这一步，后边不再更改变量，最后用left_join进行联合得到结果
t_con_data1 <- award_data[award_type=='一等奖'|award_type=='二等奖'|award_type=='三等奖',c(11,3,4,10,9)] %>% set_colnames(c('id','name','affiliation','year','award_type'))
t_con_data2 <- award_data[award_type=='一等奖'|award_type=='二等奖'|award_type=='三等奖',c(11,5,6,10,9)] %>% set_colnames(c('id','name','affiliation','year','award_type'))
t_con_data3 <- award_data[award_type=='一等奖'|award_type=='二等奖'|award_type=='三等奖',c(11,7,8,10,9)] %>% set_colnames(c('id','name','affiliation','year','award_type'))

# 另外在加个问题，由于REMap和shinyWidgets冲突，且不支持动态数据，因此改用ggplot2，但是ggplot2用的省份是全称，
# 因此对university_inf.csv的数据进行了修改，同时再此对连接变量作选择
continue_data <- rbind(t_con_data1,t_con_data2,t_con_data3) %>% left_join(.,university_inf[,c(1,3)],by='affiliation') %>% data.table()
continue_data$member_id <- 1:nrow(continue_data)

### 第一步
# 按name和affiliation，查找完全不会重复的数据，按member_id，满足条件的留下，不满足的接着进行下一步
# 这里用log1作为临时分割标记，1代表重复次数为1，0代表大于一次，
# 当然这里的大于一次不代表重复次数不为一次，很有可能出现了可辨别的同名情况，如2004年山大的张三和2017年山大的张三
# 另外添加而member_unique是参赛人非重复标记，即在本例中认为是同一个人的，他们的member_unique标记相同，不同人的则不同
# 换句话说，member_id统计人次，member_unique统计个体人
# 这里每一步对continue_data进行行分割简化，但不改变其变量结构，每一步得到的信息单独储存在有member_id/log/member_unique的数据框中
log1 <- as.numeric(!(or(duplicated(continue_data,by=c('name','affiliation'),fromLast=T),duplicated(continue_data,by=c('name','affiliation'),fromLast=F))))
continue_data1 <- cbind(continue_data,data.frame('log1'=log1))
continue_data1$member_unique1 <- log1
continue_info1 <- continue_data1[log1==1,c(7,8,9)] %>% set_colnames(c('member_id','continue_time','member_unique'))#阶段信息
mebber_count1 <- nrow(continue_info1)
continue_info1$member_unique <- 1:mebber_count1
continue_data2 <- continue_data1[log1==0,-c(8,9)]     #简化后数据，进入下一步分析中
continue_data2 <- dplyr::arrange(continue_data2,year) %>% data.table()    #为第二步用，按年排序避免乱序数据影响
continue_data2$ar <- 0                                                    #为第二步用，在函数中，判断是否已经处理过

### 第二步
# 定义一个递归函数，在continue_data2中搜索，连续年份获奖的记录，输出这些原始记录
# 这里一开始想错了，不要让递归函数干太多事，会很麻烦，让它输出原始记录，再在外面处理数据即可
# 强烈注意的点，困扰了我半天的问题，经过修改，当前函数有如下保守判断规则：
#           1、若出现如下情况，则判定1、2是同一个人，3、4、5是同一个人
#                      id name        affiliation year award_type province member_id
#                 1:  775 周林 解放军信息工程大学 2006     三等奖   河南省     19981
#                 2: 1018 周林 解放军信息工程大学 2007     一等奖   河南省      1018
#                 3:  893 周林 解放军信息工程大学 2007     二等奖   河南省     20099
#                 4: 1543 周林 解放军信息工程大学 2008     三等奖   河南省      1543
#                 5: 2573 周林 解放军信息工程大学 2009     三等奖   河南省     21779
#           2、若出现如下情况，则判定1、3、4是同一个人，2是一个人
#                       id name      affiliation year award_type province member_id
#                 1: 10672 张洋 国防科学技术大学 2014     三等奖   湖南省     28477
#                 2:  9559 张洋 国防科学技术大学 2014     二等奖   湖南省     46570
#                 3: 19237 张洋 国防科学技术大学 2015     二等奖   湖南省     30835
#                 4: 21496 张洋 国防科学技术大学 2016     二等奖   湖南省     13603
find_t_u <- function(data,item){
  if(item$ar==1) return()
  out <- NULL
  titem <- data[name==item$name&affiliation==item$affiliation&year==item$year+1,]
  if(nrow(titem)==0){
    return(item)
  }
  else if(nrow(titem)==1){
    out <- rbind(item,find_t_u(data,titem))
    return(out)
  } else{
    out <- rbind(item,titem[1,])
    return(out)
  }
}
# 程序主体，对continue_data2中的所有数据进行循环查找，当然为了简化，重复的next，得到和continue_info1相同格式的结果
# 经过处理，相同的人有相同的member_unique标记，且continue_time从1开始升序排列，即保证每一年每个成员的连续获奖次数是对的
alr <- NULL
continue_info2 <- NULL
for(i in continue_data2$member_id){
  if(i %in% alr) next
  mebber_count <- mebber_count+1
  item <- continue_data2[member_id==i,]
  tcon <- find_t_u(continue_data2,item)
  tinfo <- data.table(member_id=tcon$member_id,
                      continue_time=1:nrow(tcon),
                      member_unique=rep(mebber_count,nrow(tcon)))
  continue_data2[member_id %in% tinfo$member_id,]$ar <- 1
  continue_info2 <- rbind(continue_info2,tinfo)
  alr <- continue_info2$member_id
}

###第三步
# 综合两步结果，左连接原始数据
# 这里再次强调continue_data变量含义，member_id是按成员id，continue_time是连续获奖次数，member_unique是获奖人去重标记，
# 由于member_unique我取了重复数据最后一个标记为1，因此可以按此变量分类，按个人最后一次连续获奖的时间进行时间序列作图
continue_info <- rbind(continue_info1,continue_info2)
continue_data <- left_join(continue_data,continue_info) %>% data.table()

# 最后还有个问题，某些队伍只有两人参赛，因此有的姓名为空（14个）但是很奇葩的是居然不是NA，所以只好手动找出来，再处理掉
rmid <- c(104,20420,38719,38845,39010,39626,39881,40421,40718,40938,41010,42522,44258,45009)
continue_data <- continue_data[!(member_id %in% rmid),]

# 储存数据
write.csv(award_data,'award_data.csv',row.names=F)
write.csv(continue_data,'continue_data.csv',row.names=F)


# # 这部分代码是在当年我还不怎么会用R时候写的，那时用惯了MATLAB，竟然在这里强制用matrix格式，哈哈哈、好青涩啊
# # 之所以没删，是想运行下对比速度，结果如下
# statis <- function(){
#   na <- paste("D:/#R/GMCM/winner-info/", sep = "",2004:2017,".csv")
#   x  <- list()
#   da <- data.frame()
#   for(i in 1:14){
#     x[[i]] <- read.csv(na[i], header = T)
#     x[[i]]$year <- i+2003
#     da     <- rbind(da,x[[i]])
#   }
#   da2  <- da[which(da$所获奖项=="一等奖"|da$所获奖项=="二等奖"|da$所获奖项=="三等奖"),]
#   conw <- data.frame(题目类型="-",队号=0,队长姓名="-",队长所在学校="-",第一队友姓名="-",
#                          第一队友所在学校="-",第二队友姓名="-",第二队友所在学校="-",所获奖项="-",
#                          year=0,no=0)
# 
#   da2$no <- c(1:dim(da2)[1])
#   con <- data.frame()
#   da3 <- rbind(as.matrix(da2[,c(11,3,4)]),as.matrix(da2[,c(11,5,6)]),as.matrix(da2[,c(11,7,8)]))
#   da4 <- as.data.frame(table(da3[,2]))
#   da5 <- da4[which(da4[,2]>2),]
#   n   <- 0
#   for(i in 1:dim(da5)[1]){
#     tnam  <- da5[i,1]
#     tno   <- da3[(which(da3[,2]==tnam)),]
#     tsub  <- as.data.frame(table(tno[,3]))
#     tsub2 <- as.matrix(tsub[which(tsub[,2]>2),1])
#     for(j in 1:dim(tsub2)[1]){
#       tsub3  <- tsub2[j]
#       tnosub <- da2[as.numeric(da3[which(da3[,2]==tnam&da3[,3]==tsub3),1]),]
#       if(dim(tnosub)[1]>2){
#         tnosub <- tnosub[order(tnosub$year),]
#         con    <- rbind(con,tnosub,conw)
#         n      <- n+1
#       }
#     }
#   }
#   cat(sprintf("研究生数学建模开展十四年以来，共有%d人次获奖",length(da3)),'\n')
#   cat(sprintf("连续三年以上获奖（三等以上）的，共有%d人",n),'\n')
#   # write.table(con,"./获奖名单/conq.csv",sep = ",",row.names = F)
# }
# system.time({
#   statis()
# })
# # 运行结果如下
# 研究生数学建模开展十四年以来，共有172854人次获奖 
# 连续三年以上获奖（三等以上）的，共有573人 
# 用户  系统  流逝 
# 60.30  0.04 61.50 

# 而新写的代码，第二部分运行时间如下，算上其他部分基本可以忽略的运行时间，大概提高了两倍的运算速度
# 这主要是因为，我大幅度的减少了for的循环次数，以及data.table的强大算力的结果
# 用户  系统  流逝 
# 28.84  0.06 28.97 