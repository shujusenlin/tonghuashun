## 字符串处理、汇总数据
library(plyr)
library(stringr)
library(sqldf)
## 爬虫相关
library(RCurl)
library(XML)
## 读取数据
library(data.table)
## 数据可视化
library(ggplot2)
library(ggthemes)
library(ggradar)
## markdown 文件生产
library(knitr)
library(rmarkdown)
## 股票专用包
library(quantmod)
setwd('D:/R语言基础教程/Lesson6')


# 爬取数据
point <- fread('index_pools_whole.txt',sep='\t',
               stringsAsFactors=FALSE,header=TRUE)[1:10,]
point $ technical <- 0
point $ funds <- 0
point $ message <- 0
point $ trade <- 0
point $ basic <- 0
for(i in 1:10){
  url <- paste('http://doctor.10jqka.com.cn/',substr(point$rcode[i],1,6),'/',sep='')
  temp <- getURL(url,.encoding='utf-8')
  doc <-htmlParse(temp)
  points <- getNodeSet(doc,'//div[@class="chart_base"]/
                       div[@class="column_3d"]/div[@class="label"]')
  points <- sapply(points,xmlValue)
  point$technical[i] <- as.numeric(substr(points[1],1,3))
  point$funds[i] <- as.numeric(substr(points[2],1,3))
  point$message[i] <- as.numeric(substr(points[3],1,3))
  point$trade[i] <- as.numeric(substr(points[4],1,3))
  point$basic[i] <- as.numeric(substr(points[5],1,3))
  print(i)
}

point$total_num <- apply(point[,7:11],1,sum)
point$point_num <- 83
point$dt <- '2018-04-09'
write.csv(point,'point_180409.csv')

##行业箱型图 
point_all <- read.csv('point_180409.csv',
               stringsAsFactors=FALSE,header=TRUE)
area_total <- ddply(point_all,.(area),summarise,num=length(name))
point_total <- sqldf('select a.*
                     from point_all a
                     inner join area_total b on a.area=b.area
                     where b.num>=100')
ggplot(data=point_total,aes(x=area,y=total_num))+geom_boxplot()+
  theme_economist()+ggtitle("主要行业得分分布图")+
  theme(axis.text.x = element_text(size=15),
        plot.title = element_text(hjust=0.5,size=25))

##雷达图
point_this <- point_all[1:5,]
p <- ggradar(plot.data = point_this[,c(2,8:12)],grid.max=10,grid.mid=5,
             grid.label.size = 7,axis.label.size = 6,group.line.width = 1.2,
             group.point.size = 3.5)+
             theme_wsj()+
             theme(panel.grid = element_blank(),
                   axis.line = element_blank(),
                   axis.text = element_blank(),
                   legend.title = element_text(unique(point_total$area)[1]))
print(p)

##K线图
getSymbols("600519.ss",src="yahoo",from="2017-01-01", to='2017-10-30')   
chartSeries(`600519.SS`)  