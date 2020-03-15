# 高校信息爬取
# 超级简单的一个网页，只用最low的爬虫手段即可
# 但是信息不全，有必要爬另行修改
# 研究院信息
# 为了统计数学建模获奖情况用
# 修改日期：2018-09-15

# 加载包
library("magrittr")
library("RCurl")
library("XML")
library("rvest")
library("stringr")
library("dplyr")

# 编写爬虫函数
gx_inf <- function(){
  gxxx <- NULL
  n    <- 1
  for(start in seq(0,840,by=20)){
    url <- paste0('https://yz.chsi.com.cn/sch/search.do?start=',start)
    headers <- c(
      "Referer"="https://yz.chsi.com.cn/sch/search.do?start=0",
      "User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"
    )
    
    debugInfo <- debugGatherer()
    handle    <- getCurlHandle(debugfunction=debugInfo$update,
                               followlocation=TRUE,
                               cookiefile="",
                               verbose = TRUE)
    content <- getURL(
      url,                            
      .opts=list(httpheader=headers), 
      .encoding="utf-8",              
      curl=handle                     
    )
    tt <- read_html(content) %>% html_table() %>% '[['(1) %>% .[,1:4]
    tt$'985院校' <- as.numeric(str_detect(tt$院校特性,'985'))
    tt$'211院校' <- as.numeric(str_detect(tt$院校特性,'211'))
    tt <- tt[,-4]
    gxxx <- rbind(gxxx,tt)
    cat(sprintf('第【%d】页，第【%d~%d】条数据，抓取完毕',n,start,start+nrow(tt)),'\n')
    n <- n+1
  }
  cat(sprintf('所有数据爬取完毕，共【%d 】条',nrow(gxxx)),'\n')
  return(gxxx)
}

# 执行爬虫程序
university_inf <- gx_inf()

# 初步进行院校省份信息匹配
award_inf <- NULL
for(year in 2004:2017){
  ta <- read.csv(paste0('https://raw.githubusercontent.com/lcpmgh/GMCM/master/winner-info/',year,'.csv'),stringsAsFactors=F)
  ta$'year' <- year
  award_inf <- rbind(award_inf,ta)
}
university_name <- unique(c(award_inf$队长所在学校,award_inf$第一队友所在学校,award_inf$第二队友所在学校))
university_loc  <- data.frame('院校名称'=university_name)

# 初步匹配结果如下
university_info_temp <- left_join(university_loc,university_inf) %>% .[,1:2]





