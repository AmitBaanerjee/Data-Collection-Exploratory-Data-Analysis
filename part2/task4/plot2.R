#Plot 2

library(ggplot2)
library(dplyr)

data_18_19<-read.csv("/Users/apple/Desktop/DIC lab 1/part2/task4/plot2.csv")
data<-rbind(filter(data_18_19,(data_18_19$YEAR==2018 & data_18_19$WEEK >=40)),filter(data_18_19,(data_18_19$YEAR==2019 & data_18_19$WEEK <=6)))
attach(data)
weekdata<-c(data$WEEK)
library(stringr)
adata<-c(data$A..Subtyping.not.Performed.)
bdata<-c(data$A..2009.H1N1.)
cdata<-c(data$A..H3.)
ddata<-c(data$H3N2v)
edata<-c(data$B)
fdata<-c(data$BVic)
gdata<-c(data$BYam)

data$WEEK<-str_pad(data$WEEK, 2, pad = "0")
new<-paste(data$YEAR,data$WEEK,sep="")
#new<-new[c(TRUE,FALSE)]
new<-factor(new, levels=unique(new))
df2 <- rbind(
  data.frame(new, "Number_of_Positive_Specimens" = adata, "type"="A..Subtyping.not.Performed."),
  data.frame(new, "Number_of_Positive_Specimens" = bdata, "type"="A..2009.H1N1."),
  data.frame(new, "Number_of_Positive_Specimens" = cdata, "type"="A..H3."),
  data.frame(new, "Number_of_Positive_Specimens" = ddata, "type"="H3N2v"),
  data.frame(new, "Number_of_Positive_Specimens" = edata, "type"="B"),
  data.frame(new, "Number_of_Positive_Specimens" = fdata, "type"="BVic"),
  data.frame(new, "Number_of_Positive_Specimens" = gdata, "type"="BYam")
  )

p<-ggplot(df2, aes(x=new, y=Number_of_Positive_Specimens, fill=type)) +
  geom_bar(stat="identity",colour="black",srt=45) + 
  scale_fill_manual("", values = c("A..2009.H1N1." = "orange1", "A..Subtyping.not.Performed." = "#f4ee42","A..H3." = "red1","B" = "#006400","BVic" = "#ADFF2F","BYam" = "#32CD32","H3N2v" = "#8A2BE2"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x = "Week",y ="Number of Positive Specimens")+
  scale_y_continuous(breaks=seq(0, 1800,200))+
  scale_x_discrete(breaks = levels(new)[c(T, rep(F, 1))])
  #scale_x_discrete(0,max(new1)) +
  #theme(legend.justification = c(1, 1), legend.position = c(1, 1))
  #scale_x_continuous(limits=c(min(new1)-0.5,max(new1)+1),
                     #breaks=(new1):(max(new1)+1))
 
  #theme(legend.position=c(1,1))
theme(legend.position=c(0.8,0.7),legend.key.width=unit(0.5,"cm"),legend.key.height =unit(0.4,"cm"))
p<-p+ggtitle('Influenza Positive Tests Reported to CDC by Public Health Laboratories,\n                      National Summary 2018-2019 Season')
print(p)