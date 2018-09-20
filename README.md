# GMCM
2004~2017年全国研究生数学建模竞赛获奖数据和可视化分析案例

此案例应用的获奖数据，源自全国研究生数据建模竞赛官网（现改名为中国研究生数学建模竞赛)，旧网址为：http://gmcm.seu.edu.cn/main.htm ，上面有数据，但登录速度超慢，新网址为：https://cpipc.chinadegrees.cn/cw/hp/4 ，但是目前还没有数据。

获奖数据经过手工整理（主要是变量格式清洗），得到以年为名称的十四个csv文件（见winner-info文件夹），每个文件都包含9个中文名变量，其中某些年份没有题目类型，用N表示，某些年份没有队号，用序数表示。另外，为了使用方便，这些数据整理被整理为一个csv文件，名为award_data.csv（见winner-info文件夹），且其中的变量改为英文形式。

我想统计一下各个省份获奖数量，因此需要获奖者所在培养单位的省份信息，但是原始数据的院校名称超级混乱，经初步整理，基本化为统一格式，但是还是有一些学校、研究所用的不是官方名字，或有些培养单位在这段时间内经历了校名变更，因此，先用爬虫获取了全国研招网上所有硕士招生单位的信息（代码见1_crawler.R），之后对比数据中的获奖单位（697个），还剩下200个左右的地址是未知的，然后进一步通过人工整理，最终得到反映院校名称其所属省份信息的文件（鉴于在获奖数据中修改太麻烦，这里的文件实际上是反映包括不规则院校名称在内的、所有存在于获奖数据中的院校名称的所属省份信息）：university_info.csv（见winner-info文件夹）。

各参赛者连续获奖次数的统计代码（2_tidy.R）和处理结果（winner-info中的continue_data.csv)已经上传，其中结果变量的名称都容易理解，除了最后三个，它们的含义分别为：member_id，不做区分的记录获奖记录中所有参赛者的唯一id；continue_time，不区分重复的记录每人连续获奖次数；member_unique，对于连续获奖者的唯一性标记，这样做主要是为了统计每年产生的连续获奖人数。

对数据处理的结果，简要介绍以下几条信息
award_data.csv，共有39060行，代表14年间，一共有这么多支队伍获奖；
continue_data.csv，共有57618行，其中有14行为空值，即一共有57604人次获奖（涉及continu_data的，都不包括成功参与奖，下同）；
length(unique(continue_data$member_unique)为52621，即在我们的分析中，认为14年间，共有52621人获奖（member_unique有删除14个空值，因此max(continue_data$member_unique)为52635，这个数是错的）；
y <- data.table(continue_data) %>% unique(by='member_unique',fromLast=T) %>% nrow(.\[continue_time>1,\])，y值为3052，说明共有3052个人，有重复获奖记录，另外的49569人，只获奖一次。

本例中的统计工作，所有信息仅为官网上公布的获奖名单，因此有必要声明几个假设： 
  1、对于“连续获奖”这个概念，本例是指除成功参与奖以为的一二三等奖，毕竟只要交了解答、不被认作抄袭就可以获得参赛奖； 
  2、鉴于信息有限，只考虑同名、同学校在连续年份的获奖情况为“连续获奖”，即忽略“山大张三在2005年获奖后转学到中大并于2006年获奖”； 
  3、鉴于信息有限，不区分同名不同人、但同校名的获奖情况，即将“山大张三于2005年获奖然后另一个也叫张三的山大学生于2006年获奖”视为连续获奖； 
  4、如果张三于2005年获奖，并于2007、2008年获奖，则将其视为两次连续获奖，第一次为1连，第二次为2连。
  5、还有两个重复的例子简化方式，详见2_tidy.R
  
可视化的shiny程序已经上传，分为一体文件3_shiny.R和分体文件（见文件夹3_visualization）。这里做了妥协，写shiny很大程度上是想用REMap，那个出来的效果太棒了，但是这个包和shinyWidgets存在接口冲突，且不能动态更新数据，没办法还是用了ggplot2，ggplot2做地理信息图，最大的问题就是数据量太大，很卡，所以tablepanel的2、3加载起来会有点慢，请耐心等待。

还有ECharts2Shiny包，用在了第一个tablepanel，出来的效果也特别棒，尤其是鼠标悬浮显示信息，而且图例部分相当于整合了常见的shiny控件，编程起来方便不少，但它唯一缺点是不灵活，很多参数不能修改，那个柱形图，要是换成ggplot2中的fill模式会更好看。

shiny在交互的时候，前端输入的数据和后台数据会有冲突，奖项选择部分，还能通过内置匹配的方式修改，但是tablepanel4那个按输入姓名或校名方式搜索的功能，真的没辙了，只能靠DT包曲线救国。另外，我快疯了，shiny写了半天结果发现不能发布到网上，因为所用rgdal::readOGR貌似不能在线读取shap文件，所以只能本地用，有时间再看看还有没有其他解决办法吧，或者，发布个没有地理信息图的简化版也没准。QAQ

这是我第一个完整的发布在github上的案例，是在15号晚上开始写的，那天是今年第十五届落户杯开赛的日子，也是想借此怀念一下曾经和队友并肩作战的时光，16号山竹台风肆虐广州，在家里写了一天没出去，后来又断断续续写到了今天，也正好是今年比赛结束的日子，但是写到现在才提交，我算是超时了，并且感到极度不痛快，今晚上我回去喝一壶闷酒...

唉、人生不值得、

2019-09-19
