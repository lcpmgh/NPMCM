### v4.0版更新内容：	

1. **更新工具和数据**：使用Centos7.9.2009平台编程，其中R语言版本为v4.1.2，Rstudio Server版本为2021.09.2 Build 382，所有涉及到的packages在使用前都已于2022年01月升级到最新版本。获奖数据更新到2021年第十八届。
2. **更改关键R包**：可视化程序中，使用REmap包代替之前版本中的rgdal包，进行获奖数据省域分布图的绘制，相比之下REmap绘制的地图有以下优点，1不再需要处理shap数据从而提高效率，2动态化地图更易于交互界面查看。
3. **更改数据中省份名称**：由于改用REmap包，由原来rgdal使用的省份长名（如“北京市”），转变为REmap包使用的省份短名（如“北京”），因此将数据1-unit_info_internet.csv和3-unit_info_dataset.csv中的省份名称改为短名，同样的，数据处理结果中，相应文件中的省份名称也转变为短名。
4. **更改可视化程序中的页面布局顺序**：编写shinyapp过程中发现，REmap包显示地图，与其他控件不兼容（推测），表现为若先点击REmap地图所在页面，则其他页面的部分控件（包括shinyWidgets控件、DT表格、reactable表格）将无法显示，此bug我暂时无法解决。权宜之计，改变之前版本的页面布局顺序，将之DT包展示的获奖数据库页面，调整为可视化程序的首页。
5. **更改可视化程序的代码结构**：简化shinyapp中的UI函数，改用uiOutput和renderUI，优点是可以根据数据内容生成控件参数值，避免每次更新数据后都要修改控件（例如UI中年份选择范围）；另外，按UI的tablepanels页面（而不是之前版本中的按数据和模块）组织代码，尽量做到将renderUI和对应数据、output函数放在一起，便于维护。
6. **部署shinyapp至云服务器**：此项目的shinyapp已部署在云服务器上，欢迎访问[NPMCM](http://47.108.64.91:3838/NPMCM)。
7. **其他细节**：在data_processing.R中补充说明了仅更新获奖数据时的执行步骤；在shiyapp中调整了队伍页面的展示图，改为获奖率、获奖数量和获奖构成相关的五个图；修改了shinyapp中的其他一些细节问题；修改了data文件夹中readme文件，添加了对该文件夹中的数据的说明。