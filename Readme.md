# 中国研究生数学建模竞赛获奖数据和可视化分析案例

本项目收集了2004年（第一届）至今中（全）国研究生数学建模竞赛获奖数据，并基于R语言，进行了数据处理和可视化的工作。

这项工作最初的起因是，作者和队友在2015、2016、2017年参赛并获奖后，好奇一共有多少人连续获奖以及他们来自于哪个省份和学校。于是2017年底，我开始收集数据、敲代码。由于当时正值毕业，也面临着从matlab转出选语言的问题，认准R语言的我便开始使用R来写这部分代码，因此当时那一版惨不忍睹。2018年9月15日，台风山竹正面袭击广州深圳，被迫呆在家里的我，发现正好是当年落户杯开赛的日子，于是我决定把这个项目重写了一遍，并发布到github上。2020开年的新冠肺炎疫情期间，不能去办公室的我，看了Hadley 大神的Advanced R一书，并整合自己2019年所学，开始重新审视自己的编程水平，于是决定再一次修改这个代码，并且决定，以后每年都要来修改一次，以见证每年收获和进步。

关于项目的几个说明：

1. 中国研究生数学建模竞赛（原名全国研究生数学建模竞赛），[官网网址（新）](https://cpipc.chinadegrees.cn/cw/hp/4 )，[官方网址（旧）](http://gmcm.seu.edu.cn/main.htm)，2004年开办，每年9月开赛、11月~12月会公布当年获奖结果，旧网址曾经有历年获奖数据，不过目前404了；
2. 官方公布的获奖名单，包含一等奖、二等奖、三等奖以及成功参与（参赛、参加）奖，但是基本上参赛、提交并未被认定为作弊就能拿到成功参与奖，所以这里将获奖名单视为参赛名单，其中的一二三等奖视为获奖；
3. 部分年份获奖名单中无题型信息，则视其题型全部为N；
4. 对于个人“连续获奖”这个概念，每个版本可能有不同算法，但基本原则是：
   - 鉴于信息有限，只考虑同名、同学校在连续年份的获奖情况为“连续获奖”，即忽略“山大张三在2005年获奖后转学到中大并于2006年获奖”
   - 鉴于信息有限，不区分同名不同人、但同校名的获奖情况，即将“山大张三于2005年获奖然后另一个也叫张三的山大学生于2006年获奖”，视为连续2次获奖 
   - 如果张三于2005年获奖，并于2007、2008年获奖，则将其视为两次连续获奖，第一次为1连，第二次为2连
5. 其他详细说明见每个版本内的README文件

研究生阶段三年的数模比赛经历，有惊喜也有遗憾，非常感谢我的队友。我只是个水平有限的R语言爱好者，写此项目也是出于对R语言的实践和喜爱，本项目仅限R语言爱好者学习，切勿用于营利或其他违法目的。同时，也欢迎提出问题、讨论和交流。

最后，希望这个项目对你有所帮助。

---

## 可视化内容展示（应用已部署：[NPMCM](http://47.108.64.91:3838/NPMCM)）

1. 数据库查询系统

![shiny_app_page_1](https://raw.githubusercontent.com/lcpmgh/NPMCM/master/v4.0/shiny_preview/shiny_app_page_1.png)

2. 按队伍的获奖统计

![shiny_app_page_2](https://raw.githubusercontent.com/lcpmgh/NPMCM/master/v4.0/shiny_preview/shiny_app_page_2.png)

3. 按成员的获奖统计

![shiny_app_page_3](https://raw.githubusercontent.com/lcpmgh/NPMCM/master/v4.0/shiny_preview/shiny_app_page_3.png)

4. 连续获奖统计

![shiny_app_page_4](https://raw.githubusercontent.com/lcpmgh/NPMCM/master/v4.0/shiny_preview/shiny_app_page_4.png)

---

## 更新日志：

### 2022-02-01 v4.0：

1. **更新工具和数据**：使用Centos7.9.2009平台编程，其中R语言版本为v4.1.2，Rstudio Server版本为2021.09.2 Build 382，Shiny Server版本为v1.5.17.973，所有涉及到的packages在使用前都已于2022年01月升级到最新版本。获奖数据更新到2021年第十八届。
2. **更改关键R包**：可视化程序中，使用REmap包代替之前版本中的rgdal包，进行获奖数据省域分布图的绘制，相比之下REmap绘制的地图有以下优点，1不再需要处理shap数据从而提高效率，2动态化地图更易于交互界面查看。
3. **更改数据中省份名称**：由于改用REmap包，由原来rgdal使用的省份长名（如“北京市”），转变为REmap包使用的省份短名（如“北京”），因此将数据1-unit_info_internet.csv和3-unit_info_dataset.csv中的省份名称改为短名，同样的，数据处理结果中，相应文件中的省份名称也转变为短名。
4. **更改可视化程序中的页面布局顺序**：编写shinyapp过程中发现，REmap包显示地图，与其他控件不兼容（推测），表现为若先点击REmap地图所在页面，则其他页面的部分控件（包括shinyWidgets控件、DT表格、reactable表格）将无法显示，此bug我暂时无法解决。权宜之计，改变之前版本的页面布局顺序，将之DT包展示的获奖数据库页面，调整为可视化程序的首页。
5. **更改可视化程序的代码结构**：简化shinyapp中的UI函数，改用uiOutput和renderUI，优点是可以根据数据内容生成控件参数值，避免每次更新数据后都要修改控件（例如UI中年份选择范围）；另外，按UI的tablepanels页面（而不是之前版本中的按数据和模块）组织代码，尽量做到将renderUI和对应数据、output函数放在一起，便于维护。
6. **部署shinyapp至云服务器**：此项目的shinyapp已部署在云服务器上，欢迎访问[NPMCM](http://47.108.64.91:3838/NPMCM)。
7. **其他细节**：在data_processing.R中补充说明了仅更新获奖数据时的执行步骤；在shiyapp中调整了队伍页面的展示图，改为获奖率、获奖数量和获奖构成相关的五个图；修改了shinyapp中的其他一些细节问题；修改了data文件夹中readme文件，添加了对该文件夹中的数据的说明。

### 2021-05-01 v3.0：

1. 更新工具和数据：R语言版本为v4.0.5，Rstudio版本为1.4.1106，所有涉及到的packages在使用前都已于2021年04月26日升级到最新版本。获奖数据更新到2020年第十七届；
2. 优化代码：将数据处理部分的代码整理到一个文件中，修改了相关函数名和变量名；
3. 优化文件（夹）名：规范了文件和文件夹的名称，在文件名中区分了读取或是生成顺序，便于查找和识别。
4. 优化文件存储路径：将获奖名单的原始数据保存在根目录下的文件夹中，今后每个版本都在此读取原始文件，将中间过程所需或所产生的数据文件保存在./shiny_app/data中，便于可视化；
5. 修复问题：进一步完善了获奖名单中，培养单位名称不统一问题，即进一步更新了0-unit_name_convert.csv文件；
6. 添加可视化图表：在shiny的第一页中添加了“获奖率年际变化“图和”获奖题型构成比例年际变化“图，其中，部分年份获奖名单中没有出现成功参与奖，则此年度获奖率空缺，此外，为避避免歧义，修改了可视化中图表的标题、图例、坐标轴名称等部分文字描述；
7. 其他：由于此版本强制规定读取和保存的csv文件为UTF-8编码，导致在我的电脑上，shiny部分第四页的获奖信息查询系统不能正常使用了，猜想是由于windows中Rstudio打开的shinyui为GB2312，在此部分查找信息时的输入文字为GB2312，与底层数据编码矛盾导致的。然而，虽然强制规定了编码，但还是由于编码问题，此shinyapp并不能上传到shinyapps.io。

### 2020-03-14 v2.0：

1. 数据更新到最近的2019年；
2. 规范、明确了整个数据处理流程，并试图兼容以后的数据更新（但是shinyui部分还做不到，现在那里的代码必须写成年份的确切数字，还没想到如何根据获奖数据自行生成）；
3. 添加了对获奖数据中，同一培养单位名称不统一的处理工作（由于这里需要人工建立名字的识别替换数据集，所以只能尽力统计，难免有遗漏）；
4. 优化代码运行效率，尽量避免循环语句和代码重复；
5. 重新设计了求连续获奖年份的算法，其核心是data.table包的分而治之方法和采用差分算法的自编函数find_series，具体内容见代码文件./v2.0/3_create_award_team_member.r；
6. 除爬虫收集的原始数据和地图数据外，所有csv文件改用GB2312编码，以避免本地运行时出现乱码；
7. 之前的代码和数据被放在v1.0文件夹中，但原始代码没有改动，修改文件夹路径会造成运行不了的情况；
8. 一些小问题，比如2013年有一位队员只有名字没有培养单位名，已经按队友的信息进行补全，且由于全数据中只有一个，所以没做代码上的识别判断，直接在原数据上修改了。

### 2018-09-19 v1.0：

1. 项目第一次修改，更新说明见./v1.0/READ_v1.0.md

### 2018-01-01 v0.5：

1. 项目创建

