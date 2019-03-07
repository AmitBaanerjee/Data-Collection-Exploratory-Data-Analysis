df<-read.csv("/Users/apple/Desktop/DIC lab 1/part2/task6/task6.csv",skip=1)
data<-df[17:69,]
library(ggplot2)
library(reshape)
yearweek<-paste(data$YEAR,data$WEEK,sep="")
yearweek<-factor(yearweek,levels=unique(yearweek))
ggplot(data=data,mapping=aes(x=yearweek))+
  geom_bar(stat="identity",aes(y=data$TOTAL.A,fill="A"),color="black")+
  geom_bar(stat="identity",aes(y=data$TOTAL.B,fill="B"),color="black")+
  geom_line(stat="identity",aes(y=(data$PERCENT.POSITIVE)*45,group=1,color="Percent Positive"),size=1)+
  geom_line(linetype="dashed",size=1,stat="identity",
            aes(y=(data$PERCENT.A)*45,group=1,color="% Positive Flu A"))+
  geom_line(linetype="dotted",size=1,stat="identity",
            aes(y=(data$PERCENT.B)*45,group=1,color="% Positive Flu B"))+
  scale_color_manual(name = "", 
                     values = c("Percent Positive"="black", 
                                "% Positive Flu A"="orange3",
                                "% Positive Flu B"="greenyellow"))+
  scale_fill_manual(name="",values=c("A"="yellow","B"="forestgreen"))+
  scale_linetype_manual(values = c(3, 3, 1))+
  guides(fill = guide_legend(order=1),
         color = guide_legend(order=2,override.aes = list(linetype = c("dashed", "dotted","solid"))))+
  theme(legend.position="right",axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks=seq(0,1600,200),limits=c(0,1600),
                     sec.axis = sec_axis(~./45,name="Percent Positive"))+
  xlab("Week")+ylab("Number of Positive Specimens")+
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\n       New York Summary,2018-2019 Season")
