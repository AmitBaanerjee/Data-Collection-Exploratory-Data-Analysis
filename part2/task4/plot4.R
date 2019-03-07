#Plot % Visits ILI

library(dplyr)
library(ggplot2)
data<-read.csv("/Users/apple/Desktop/DIC lab 1/part2/task4/plot4.csv",skip=1)
data_09_10 <-rbind(filter(data,(data$YEAR==2009 & data$WEEK >= 40)),filter(data,(data$YEAR==2010 & data$WEEK <= 39)))
data_11_12<-rbind(filter(data,(data$YEAR==2011 & data$WEEK >= 40)),filter(data,(data$YEAR==2012 & data$WEEK <= 39)))
data_14_15<-rbind(filter(data,(data$YEAR==2014 & data$WEEK >= 40)),filter(data,(data$YEAR==2015 & data$WEEK <= 38)))
data_15_16<-rbind(filter(data,(data$YEAR==2015 & data$WEEK >= 40)),filter(data,(data$YEAR==2016 & data$WEEK <= 39)))
data_16_17<-rbind(filter(data,(data$YEAR==2016 & data$WEEK >= 40)),filter(data,(data$YEAR==2017 & data$WEEK <= 39)))
data_17_18<-rbind(filter(data,(data$YEAR==2017 & data$WEEK >= 40)),filter(data,(data$YEAR==2018 & data$WEEK <= 39)))
data_18_19<-rbind(filter(data,(data$YEAR==2018 & data$WEEK >= 40)),filter(data,(data$YEAR==2019 & data$WEEK <= 39)))

weekdata<-c(data_09_10$WEEK)
weekdata1<-c(data_18_19$WEEK)

library(stringr)
percent_ili_visits_09_10<-c(data_09_10$X..WEIGHTED.ILI)
percent_ili_visits_11_12<-c(data_11_12$X..WEIGHTED.ILI)
percent_ili_visits_14_15<-c(data_14_15$X..WEIGHTED.ILI)
percent_ili_visits_15_16<-c(data_15_16$X..WEIGHTED.ILI)
percent_ili_visits_16_17<-c(data_16_17$X..WEIGHTED.ILI)
percent_ili_visits_17_18<-c(data_17_18$X..WEIGHTED.ILI)
percent_ili_visits_18_19<-c(data_18_19$X..WEIGHTED.ILI)

weekdata<-factor(weekdata, levels=unique(weekdata))
weekdata1<-factor(weekdata1, levels=unique(weekdata1)) 

national_baseline<-rep.int(2.2,52)
national_baseline_data=cbind(data.frame(weekdata),data.frame(national_baseline))

ggplot()+ 
  geom_line(data=data_09_10, aes(x=weekdata, y=percent_ili_visits_09_10,group = 1,color="2009-2010 season"), size=1)+
  geom_line(data=data_11_12, aes(x=weekdata, y=percent_ili_visits_11_12,group = 1,color="2011-2012 season"), size=1)+
  geom_line(data=data_14_15, aes(x=weekdata, y=percent_ili_visits_14_15,group = 1,color="2014-2015 season"), size=1)+
  geom_line(data=data_15_16, aes(x=weekdata, y=percent_ili_visits_15_16,group = 1,color="2015-2016 season"), size=1)+
  geom_line(data=data_16_17, aes(x=weekdata, y=percent_ili_visits_16_17,group = 1,color="2016-2017 season"), size=1)+
  geom_line(data=data_17_18, aes(x=weekdata, y=percent_ili_visits_17_18,group = 1,color="2017-2018 season"), size=1)+
  geom_line(data=data_18_19, aes(x=weekdata1, y=percent_ili_visits_18_19,group = 1,color="2018-2019 season"), size=1)+
  geom_point(data=data_18_19, aes(x=weekdata1, y=percent_ili_visits_18_19,group = 1,color="2018-2019 season"),stroke=1,shape=24, fill="red", color="black", size=1)+
  geom_line(data=national_baseline_data,aes(x=national_baseline_data$weekdata, y=national_baseline_data$national_baseline,group = 1,color="National Baseline"),linetype="dashed")+
  guides(color = guide_legend(order=2,override.aes = list(linetype = c("solid", "solid","solid","solid","solid","solid","solid","dashed"))))+
  scale_x_discrete(breaks = levels(weekdata)[c(T, rep(F, 1))])+
  scale_y_continuous(breaks=seq(0.0,8.0,1))+
  scale_color_manual(name = "", values = c("2009-2010 season"="grey", "2011-2012 season"="green","2014-2015 season"="pink","2015-2016 season"="orange","2016-2017 season"="blue","2017-2018 season"="cyan1","2018-2019 season"="red","National Baseline"="black"))+
  scale_shape_manual(values=c("2018-2019 season"="red"))+
  labs(x = "Week",y = "% of Visits for ILI")+
  ggtitle('                  Percentage of Visits for Influenza-like-Illness(ILI) Reported by\n                  the U.S. Outpatient Influenza-like-Illness Surveillance Network(ILINet),\n                 Weekly National Summary,2018-2019 and Selected Previous Seasons')+
  theme(legend.position=c(0.9,0.6),legend.background = element_rect(color = "black"))