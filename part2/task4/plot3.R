#Reference:- https://www.r-bloggers.com/us-state-maps-using-map_data/

library(ggplot2)
library(maps)
library(ggmap)
library(usmap)
states<-us_map(regions="states")
data<-read.csv("/Users/apple/Desktop/DIC lab 1/part2/task4/plot3.csv")
data<-data[866:919,]
colnames(states)[9] <- "STATENAME"
df1<-merge(states,data,by="STATENAME")
df1<-df1[df1$STATENAME!="District of Columbia",]
test<-gsub("Level ","",df1$ACTIVITY.LEVEL) 
test<-as.numeric(test)
df2<-cbind(df1,test)
p<-ggplot()
p<-p + geom_polygon(data=df2,aes(x=long,y=lat,group=group,fill=df2$test),colour="black",size=0.2)+
  scale_fill_continuous(low="chartreuse",high="red1",guide=guide_legend(reverse=TRUE,keywidth = 1,default.unit = "line"),
                        breaks=c(0,1,2,3,4,5,6,7,8,9,10),
                        labels=c("0","1","2","3","4","5","6","7","8","9","10"))
P1 <- p + theme_bw()  + labs(fill = "ILI Activity Level" 
                             ,title = "2018-19 Influenza Season Week 4 ending Jan 26,2019", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())