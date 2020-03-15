针对此文件夹中的数据说明：	

 1.   以年份数字为名字的csv文件：经过统一列名、列顺序的原始获奖数据，用于制作total_data.csv
 2.   award_member.csv：按队员整理的获奖数据，用于可视化
 3.   award_team.csv：按队伍整理的获奖数据，用于可视化
 4.   location_convert.csv：省份名的长名和短名对应关系，爬虫结果是短名，但是ggplot中需要长名
 5.   province_capital.csv：各省省会经纬度，可视化会在图中标记数量，位置采用省会坐标
 6.   total_data.csv：经过汇总的获奖数据，用于生成award_member.csv和award_team.csv
 7.   unit_location_0.csv：网络爬虫结果，由于爬虫保存的为utf-8，这里需要储存，然后用第三方软件修改为GB2312
 8.   unit_location_dataset.csv：培养单位地理位置数据库，整合自爬虫结果和获奖名单，有冗余行，用来为获奖者添加位置信息
 9.   unit_name_identity_dataset.csv：培养单位名称对应数据库，用来规范total_data.csv和unit_location_dataset.csv中培养单位名称
 10. front：字体文件夹，里面是复制自系统的微软雅黑标准字体，用于可视化
 11. mapdata：中国地图shap文件，用于可视化（注意R语言rgdal包识别不到澳门，影响不大，因为会有ggrepel进行标注）
  12.   所有csv文件均为GB2312编码