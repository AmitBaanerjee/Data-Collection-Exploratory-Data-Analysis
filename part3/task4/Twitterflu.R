#Name-Amit Banerjee
#Project Partner-Yogesh Sawant

library(rtweet)
library(ggplot2)
library(maps)
library(usmap)
library(ggmap)
library(dplyr)
register_google(key = 'AIzaSyAqU3dQ5RUHNQJ5Tnl0Odyp1bk2Jiu6bmA') 
create_token(
  app = "DIC lab 1",
  consumer_key = "jK2bsMkRHNXthROyrplDieZaV",
  consumer_secret = "4mjuIGQWkBtZJVq1ffyN4b9GCty6ChDRJxdUBFzxRUyqzHPOUw",
  access_token = "128911319-6epULaDIDCVxrjPxB29LHPvOH0vnhnvvdscGRH9n",
  access_secret = "xnZvtP6ZUaMDNoVeCiUSmXDL9fv7cqZqipoUdzPcc54L0")
rt <- search_tweets(q="flu",geocode = lookup_coords("usa"), n = 3000,include_rts = FALSE)
rtinf <- search_tweets(q="influenza",geocode = lookup_coords("usa"), n = 3000,include_rts = FALSE)
rtcough<-search_tweets(q="cough",geocode = lookup_coords("usa"), n = 3000,include_rts = FALSE)
rt1<-rbind(rt,rtinf,rtcough)
rt <- lat_lng(rt1)
rt1<-unique(rt)
geodf<-geocode(rt1$location)
rt2<-cbind(rt1,geodf)
rt3 <- rt2[ -c(89,90) ]
rt4 <- filter(rt3,lon!="", lat!="")
#REFERENCE:-https://stackoverflow.com/questions/45723974/extracting-city-and-state-information-from-a-google-street-address
revdf <- do.call(rbind,
                          lapply(1:nrow(rt4),
                                 function(i)revgeocode(as.numeric(rt4[i,89:90]))))
rt5<-cbind(rt4,revdf)
library(stringr)
#https://stackoverflow.com/questions/25299470/regular-expression-to-split-up-city-state
rt6 <- str_extract(revdf, "([A-Z]{2})")
rt7<-state.name[match(rt6,state.abb)]
rt8<-cbind(rt5,rt7)
rt8<-rt8[!is.na(rt8$rt7),]
colnames(rt8)[92] <- "statename"
colnames(rt8)[91] <- "Full Address"
test<-cbind(rt8[1],rt8[2],rt8[3],rt8[4],rt8[5],rt8[6],rt8[89],rt8[90],rt8[91],rt8[92])
write.csv(test,"/Users/apple/Desktop/DIC lab 1/part3/task4/finaltweets.csv")

df<-read.csv("/Users/apple/Desktop/DIC lab 1/part3/task4/finaltweets.csv")
states<-df[11]
final<-(table(states))
final<-data.frame(location=names(final),frequency=c(final))

states<-us_map(regions="states")
colnames(final)[1]<-'full'
#Merging state data and final DF using outer join to cover all states
finaldf<-merge(states,final,by="full",all.x=TRUE)
finaldf<-finaldf[finaldf$full!="District of Columbia",]
finaldf2<-finaldf[order(finaldf$order),]
finaldf2[["frequency"]][is.na(finaldf2[["frequency"]])] <- 0
#Plotting the heat map
p<-ggplot()
p<-p + geom_polygon(data=finaldf2,aes(x=long,y=lat,group=group,fill=finaldf2$frequency),size=0.2,color="black")+
  scale_fill_continuous(low="yellow",high="blue",guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Frequency of Tweets" 
                             ,title = "                      2019 All Tweets Distribution",
                             subtitle="\nTotal No. of Unique Tweets:  6532 \nHighest No. of Tweets from:  California \nLowest No. of Tweets from:  Hawaii  \nIt can be observed that majority of tweets are shared by big states such as california,Texas,Kansas \nand New York,however flu cases were highest in Texas according to CDC heatmap",x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
