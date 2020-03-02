library(caret)

library(dplyr)

library(ggplot2)
library(ggpubr)

daysData <- function(dataset,dayname){
  
  dataset$daynames = format(dataset$date,"%a")
  print(head(dataset$daynames))
  switch (dayname,
          MON = {
            dataset = with(dataset,dataset[(daynames == "Mon"),])
            
          },
          TUE ={
            dataset = with(dataset,dataset[(daynames == "Tue"),])
          },
          WED ={
            dataset = with(dataset,dataset[(daynames == "Wed"),])
            
          },
          THU = {
            dataset = with(dataset,dataset[(daynames == "Thu"),])
          },
          FRI = {
            dataset = with(dataset,dataset[(daynames == "Fri"),])
          },
          SAT = {
            dataset = with(dataset,dataset[(daynames == "Sat"),])
          },
          SUN = {
            dataset = with(dataset,dataset[(daynames == "Sun"),])
          },
          WEEKDAY ={
            dataset = dataset[c(which(dataset$daynames %in% c('Mon','Tue','Wed','Thu','Fri'))),]
            
          },
          WEEKEND = {
            dataset = dataset[c(which(dataset$daynames %in% c('Sat','Sun'))),]
          }
  )
  return(dataset)
} 





myDatesDF$date= as.POSIXct(myDatesDF$event_time,format='%Y-%m-%d',tz= "UTC")

myDatesDF$percentageVariation = (100 - ((myDatesDF$V/230) * 100))
myDatesDF$percentageVariation = abs(myDatesDF$percentageVariation)
myDatesDF$categoryVariation = ifelse(myDatesDF$percentageVariation < 5 ,0 , ifelse(myDatesDF$percentageVariation >= 5 & myDatesDF$percentageVariation < 8, 5,ifelse(myDatesDF$percentageVariation >= 8 & myDatesDF$percentageVariation < 10,8,ifelse(myDatesDF$percentageVariation >= 10 & myDatesDF$percentageVariation < 12, 10,12))))
myDatesDF$limitcrossed=ifelse(myDatesDF$percentageVariation<10,"NO DANGER","DANGER")


g1 <- ggplot(myDatesDF, aes(myDatesDF$event_time,group=myDatesDF$limitcrossed,col=limitcrossed))
g1 <- g1 + geom_point(aes(y=myDatesDF$V))
g1 + labs(x = "days",y="voltage variation",title = "daysname vs voltage variation")



myDatesDF=myDatesDF[ order(myDatesDF$event_time , decreasing = FALSE ),]
myDatesDF$cummulative_sum=cumsum(myDatesDF$Eact)
g1 <- ggplot(myDatesDF, aes(myDatesDF$event_time))
g1 <- g1 + geom_line(aes(y=I), colour="blue")
g1 + labs(x = "TIME",y="I",title = "CURRENT")




monday = daysData(myDatesDF,"MON")
tuesday = daysData(myDatesDF,"TUE")
wednesday=daysData(myDatesDF,"WED")
thursday=daysData(myDatesDF,"THU")
friday=daysData(myDatesDF,"FRI")
saturday=daysData(myDatesDF,"SAT")
sunday=daysData(myDatesDF,"SUN")



monday=monday[ order(monday$event_time , decreasing = FALSE ),]
monday$cummulative_sum=cumsum(monday$Eact)
g1 <- ggplot(monday, aes(monday$event_time))
g1 <- g1 + geom_line(aes(y=Eact), colour="blue")
g1 + labs(x = "TIME",y="ENERGY",title = "Power plot")
#barplot()


tuesday=tuesday[ order(tuesday$event_time , decreasing = FALSE ),]
tuesday$cummulative_sum=cumsum(tuesday$Eact)
g2 <- ggplot(tuesday, aes(tuesday$event_time))
g2 <- g2 + geom_line(aes(y=cummulative_sum), colour="blue")
g2 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")



wednesday=wednesday[ order(wednesday$event_time , decreasing = FALSE ),]
wednesday$cummulative_sum=cumsum(wednesday$Eact)
g3 <- ggplot(wednesday, aes(wednesday$event_time))
g3 <- g3 + geom_line(aes(y=cummulative_sum), colour="blue")
g3 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")



thursday=thursday[ order(thursday$event_time , decreasing = FALSE ),]
thursday$cummulative_sum=cumsum(thursday$Eact)
g4 <- ggplot(thursday, aes(thursday$event_time))
g4 <- g4 + geom_line(aes(y=cummulative_sum), colour="blue")
g4 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")


friday=friday[ order(friday$event_time , decreasing = FALSE ),]
friday$cummulative_sum=cumsum(friday$Eact)
g5 <- ggplot(friday, aes(friday$event_time))
g5 <- g5 + geom_line(aes(y=cummulative_sum), colour="blue")
g5 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")


saturday=saturday[ order(saturday$event_time , decreasing = FALSE ),]
saturday$cummulative_sum=cumsum(saturday$Eact)
g6 <- ggplot(saturday, aes(saturday$event_time))
g6 <- g6 + geom_line(aes(y=cummulative_sum), colour="blue")
g6 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")


sunday=sunday[ order(sunday$event_time , decreasing = FALSE ),]
sunday$cummulative_sum=cumsum(sunday$Eact)
g7 <- ggplot(sunday, aes(sunday$event_time))
g7 <- g7 + geom_line(aes(y=cummulative_sum), colour="blue")
g7 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")

figure=ggarrange(g1,g2,g3,g4,g5,g6,g7,labels=c("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),ncol=3,nrow=3)
figure

##############################################################################################
