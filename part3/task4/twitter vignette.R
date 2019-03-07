install.packages('twitteR', repos='https://cran.r-project.org')
install.packages('ROAuth', repos = 'https://cran.r-project.org')
library(twitteR)
library(ggplot2)
library(maps)
library(usmap)
library(ggmap)
register_google(key = 'AIzaSyAqU3dQ5RUHNQJ5Tnl0Odyp1bk2Jiu6bmA') 
appname <- "dic project 1"
apikey <- "yCSXpwrO1Y4Jsy6fegGOGQCkU"
apisecret <- "QKj5xAQpV9PYqxAAckFGNYp3qDO7KepmC7vIi0Spz1RN8pBhai"
token <- "128911319-Kfn1lFaMiluelOOq3PlNPpYHgyJrlydKsITvo1Cv"
tokensecret <- "ifyZJOidCTBFQZoUqnoOBGNkCpibH8714gkMuqEbWMR8C"
setup_twitter_oauth(apikey,apisecret,token,tokensecret)

tweets<-searchTwitter("flu",n=10000)
noretweets<-strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
twdf<-twListToDF(noretweets)
usernames<-lookupUsers(twdf$screenName)
userdf<-twListToDF(usernames)
userloc<-userdf[,c(11,12)]
userloc[userloc==""]<-NA
userloc<-na.omit(userloc)
library(ggmap)
temp<-na.omit(cbind(userloc,geocode(userloc$location)))
write.csv(twdf,"/Users/apple/Desktop/DIC lab 1/part3/task4/Finaltweets.csv")
write.csv(temp,"/Users/apple/Desktop/DIC lab 1/part3/task4/userdata.csv")

twdf<-read.csv("/Users/apple/Desktop/DIC lab 1/part3/task4/Finaltweets.csv")
temp<-read.csv("/Users/apple/Desktop/DIC lab 1/part3/task4/userdata.csv")
library(sp)
library(maps)
library(maptools)
temp<-temp[temp[4]<0,]
longlat<-data.frame(temp$lon,temp$lat)
#REFERENCE:- https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

test<-data.frame(latlong2state(longlat))
test<-na.omit(test)   
final<-(table(test))
final<-data.frame(location=names(final),frequency=c(final))

states<-us_map(regions="states")
states$full<-tolower(states$full)
colnames(final)[1]<-'full'
finaldf<-merge(states,final,by="full",all.x=TRUE)
finaldf<-finaldf[finaldf$full!="District of Columbia",]
finaldf2<-finaldf[order(finaldf$order),]
finaldf2[["frequency"]][is.na(finaldf2[["frequency"]])] <- 0
p<-ggplot()
p<-p + geom_polygon(data=finaldf2,aes(x=long,y=lat,group=group,fill=finaldf2$frequency),size=0.2,color="black")+
  scale_fill_continuous(low="chartreuse",high="red1",guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Frequency of Tweets" 
                             ,title = "                      2019 Flu Tweet Distribution",
                             subtitle="\nIt can be seen that majority of tweets are shared by big cities such as california,Texas \nand New York even when flu cases were highest in Texas according to CDC heatmap",x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
