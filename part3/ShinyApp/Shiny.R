#Name-Amit Banerjee
#Project Partner-Yogesh Sawant
#SHINY APP URL:- https://yogesh-amit.shinyapps.io/dic_heatmap_deployment/
library(shiny)
library(usmap)
library(ggplot2)
ui<-fluidPage(
  titlePanel("Comparative Analysis"),
  
  sidebarLayout(position = "left",
                
                sidebarPanel("",
                             radioButtons("radio", label = h4("Choose a Graph to Display"),
                                          choices = list("CDC" = "cdc", "CDC vs Twitter All" = "TwtFLu", "CDC vs Twitter Influencza & Flu" = "TwtInFLu"), 
                                          selected = "cdc")),
                mainPanel(
                  fluidRow(
                    column(6,plotOutput(outputId="plotgraph1", width="700px",height="500px")),
                    column(12,plotOutput(outputId="plotgraph2", width="750px",height="550px"))
                  )
                  
                )
  )
)

data<-read.csv("/Users/apple/Desktop/DIC lab 1/part3/ShinyApp/plot3.csv")
states<-us_map(regions="states")
data<-data[866:919,]
colnames(states)[9] <- "STATENAME"
df1<-merge(states,data,by="STATENAME")
df1<-df1[df1$STATENAME!="District of Columbia",]
test<-gsub("Level ","",df1$ACTIVITY.LEVEL) 
test<-as.numeric(test)
df2<-cbind(df1,test)
#cdc end

#twitter flu start
df<-read.csv("/Users/apple/Desktop/DIC lab 1/part3/ShinyApp/finaltweets.csv")
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
#twitter flu end

#twitter influenza start
twtinfl=function(){
  df<-read.csv("/Users/apple/Desktop/DIC lab 1/part3/ShinyApp/finaltweets_task5.csv")
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
  p<-ggplot()
  p<-p + geom_polygon(data=finaldf2,aes(x=long,y=lat,group=group,fill=finaldf2$frequency),size=0.2,color="black")+
    scale_fill_continuous(low="yellow",high="blue",guide="colorbar")
  P1 <- p + theme_bw()  + labs(fill = "Frequency of Tweets" 
                               ,title = "2019 Influenza & Flu Tweet Distribution",
                               subtitle="\nTotal No. of Unique Tweets:  4075 \nHighest No. of Tweets from:  California and Kansas \nLowest No. of Tweets from:  Hawaii  \nIt can be observed that majority of tweets are shared by big states such as california,Texas,Kansas \nand New York,however Influenza cases were highest in Texas according to CDC heatmap",x="", y="")
  plttwtinfl<-P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  return (plttwtinfl)
}

server<-function(input, output) 
{
  set.seed(1234)
  pt <- reactive({
    input$radio 
    if(input$radio=="cdc")
    {
      p<-ggplot()
      p<-p + geom_polygon(data=df2,aes(x=long,y=lat,group=group,fill=df2$test),colour="black",size=0.2)+
        scale_fill_continuous(low="chartreuse",high="red1",guide=guide_legend(reverse=TRUE,keywidth = 1,default.unit = "line"),
                              breaks=c(0,1,2,3,4,5,6,7,8,9,10),
                              labels=c("0","1","2","3","4","5","6","7","8","9","10"))
      P1 <- p + theme_bw()  + labs(fill = "ILI Activity Level"
                                   ,title = "2018-19 Influenza Season Week 4 ending Jan 26,2019", x="", y="")
      pt1<-P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
      pt2<-NULL
      output$plotgraph1 = renderPlot({pt1}) 
      return(pt2)
    }
    if(input$radio=="TwtFLu")
    {
      p1<-ggplot()
      p1<-p1 + geom_polygon(data=df2,aes(x=long,y=lat,group=group,fill=df2$test),colour="black",size=0.2)+
        scale_fill_continuous(low="chartreuse",high="red1",guide=guide_legend(reverse=TRUE,keywidth = 1,default.unit = "line"),
                              breaks=c(0,1,2,3,4,5,6,7,8,9,10),
                              labels=c("0","1","2","3","4","5","6","7","8","9","10"))
      P1 <- p1 + theme_bw()  + labs(fill = "ILI Activity Level"
                                    ,title = "2018-19 Influenza Season Week 4 ending Jan 26,2019", x="", y="")
      pt1<-P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
      ######
      output$plotgraph1 = renderPlot({pt1}) 
      p<-ggplot()
      p<-p + geom_polygon(data=finaldf2,aes(x=long,y=lat,group=group,fill=finaldf2$frequency),size=0.2,color="black")+
        scale_fill_continuous(low="yellow",high="blue",guide="colorbar")
      P1 <- p + theme_bw()  + labs(fill = "Frequency of Tweets"
                                   ,title = "2019 All Tweet Distribution",
                                   subtitle="\nTotal No. of Unique Tweets: 6532   \nHighest No. of Tweets from:  California \nLowest No. of Tweets from:  Hawaii  \nIt can be observed that majority of tweets are shared by big states such as california,Texas,Kansas \nand New York,however flu cases were highest in Texas according to CDC heatmap",x="", y="")
      pt2<-P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
      
      return(pt2)
    }
    if(input$radio=="TwtInFLu")
    {
      p1<-ggplot()
      p1<-p1 + geom_polygon(data=df2,aes(x=long,y=lat,group=group,fill=df2$test),colour="black",size=0.2)+
        scale_fill_continuous(low="chartreuse",high="red1",guide=guide_legend(reverse=TRUE,keywidth = 1,default.unit = "line"),
                              breaks=c(0,1,2,3,4,5,6,7,8,9,10),
                              labels=c("0","1","2","3","4","5","6","7","8","9","10"))
      P1 <- p1 + theme_bw()  + labs(fill = "ILI Activity Level"
                                    ,title = "2018-19 Influenza Season Week 4 ending Jan 26,2019",x="", y="")
      
      pt1<-P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
      ######
      output$plotgraph1 = renderPlot({pt1}) 
      pt2=twtinfl()
      return(pt2)
    }
    else {
      return(list(NULL,NULL))
    }
  })
  
  #output$plotgraph1 = renderPlot({x})
  #output$plotgraph2 = renderPlot({y})
  output$plotgraph2 = renderPlot({pt()})
}
app<-shinyApp(ui = ui, server = server)

runApp(app)
