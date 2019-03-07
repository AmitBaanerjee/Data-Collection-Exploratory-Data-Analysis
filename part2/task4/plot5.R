data<-read.csv("/Users/apple/Desktop/DIC lab 1/part2/task4/plot5.csv")
library(ggplot2)
data<-data[1:175,]
library(dplyr)
report1<-filter(data,data$SEASON=="2015-16")
sum1<-sum(report1$NO..OF.DEATHS)
report2<-filter(data,data$SEASON=="2016-17")
sum2<-sum(report2$NO..OF.DEATHS)
report3<-filter(data,data$SEASON=="2017-18")
sum3<-sum(report3$NO..OF.DEATHS)
report4<-filter(data,data$SEASON=="2018-19")
sum4<-sum(report4$NO..OF.DEATHS)
ggplot(data=data,mapping=aes(x=data$WEEK.NUMBER))+
  geom_bar(stat="identity",aes(y=data$PREVIOUS.WEEK.DEATHS,fill="Deaths Reported Previous Week"),color="black")+
  geom_bar(stat="identity",aes(y=data$CURRENT.WEEK.DEATHS,fill="Deaths Reported Current Week"),color="black")+
  scale_x_discrete(breaks = levels(data$WEEK.NUMBER)[c(T, rep(F, 5))])+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(name = "", values = c("Deaths Reported Previous Week" = "forestgreen","Deaths Reported Current Week"="cyan"))+
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.box.background = element_rect(colour = "black"))+
  xlab("Week of Death")+ylab("Number of Deaths")+
  scale_y_continuous(breaks=seq(0,30,5),limits=c(0,30))  +
  ggtitle("          Number of Influenza-Associated Pediatric Deaths\n            by week of Death:2015-2016 season to present")+
  annotate("text", x = 22, y = 24, label = "2015-16",size=4)+
  annotate("text",x=21,y=22,label="Number of Deaths",size=3)+
  annotate("text",x=22,y=20,label=paste("Reported:",sum1,sep=""),size=3)+
  annotate("text",x=73,y=24,label="2016-17",size=4)+
  annotate("text",x=72,y=22,label="Number of Deaths",size=3)+
  annotate("text",x=73,y=20,label=paste("Reported:",sum2,sep=""),size=3)+
  annotate("text",x=120,y=24,label="2017-18",size=4)+
  annotate("text",x=119,y=22,label="Number of Deaths",size=3)+
  annotate("text",x=120,y=20,label=paste("Reported:",sum3,sep=""),size=3)+
  annotate("text",x=160,y=24,label="2018-19",size=4)+
  annotate("text",x=160,y=22,label="Number of Deaths",size=3)+
  annotate("text",x=160,y=20,label=paste("Reported:",sum4,sep=""),size=3)
