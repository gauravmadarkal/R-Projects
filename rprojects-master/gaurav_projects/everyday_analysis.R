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



energydataset1=read.csv("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\gaurav_data.csv")
energydataset1_renew=energydataset1[,-c(15)]
energydataset1_renew=na.omit(energydataset1_renew)
colnames(energydataset1_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
energydataset1_renew=subset(energydataset1_renew,Eact>=0)
energydataset1_renew$event_time= strptime(energydataset1_renew$event_time,"%Y-%m-%d %H:%M:%OS")
#to print only date then use ,as.Date(energydataset1_renew$event_time, "%Y-%m-%d")
#plot(energydataset1_renew$event_time, energydataset1_renew$switch,pch = 16,cex.axis = 0.5,col = "blue")
#lines(energydataset1_renew$event_time,energydataset1_renew$switch , col = "steelblue")
energydataset1_renew$event_time=as.POSIXct(energydataset1_renew$event_time)

energydataset1_renew$date= as.POSIXct(energydataset1_renew$event_time,format='%Y-%m-%d',tz= "UTC")

energydataset1_renew$percentageVariation = (100 - ((energydataset1_renew$V/230) * 100))
energydataset1_renew$percentageVariation = abs(energydataset1_renew$percentageVariation)
energydataset1_renew$categoryVariation = ifelse(energydataset1_renew$percentageVariation < 5 ,0 , ifelse(energydataset1_renew$percentageVariation >= 5 & energydataset1_renew$percentageVariation < 8, 5,ifelse(energydataset1_renew$percentageVariation >= 8 & energydataset1_renew$percentageVariation < 10,8,ifelse(energydataset1_renew$percentageVariation >= 10 & energydataset1_renew$percentageVariation < 12, 10,12))))
energydataset1_renew$limitcrossed=ifelse(energydataset1_renew$percentageVariation<10,"NO DANGER","DANGER")


g1 <- ggplot(energydataset1_renew, aes(energydataset1_renew$event_time,group=energydataset1_renew$limitcrossed,col=limitcrossed))
g1 <- g1 + geom_point(aes(y=energydataset1_renew$V))
g1 + labs(x = "days",y="voltage variation",title = "daysname vs voltage variation")



energydataset1_renew=energydataset1_renew[ order(energydataset1_renew$event_time , decreasing = FALSE ),]
energydataset1_renew$cummulative_sum=cumsum(energydataset1_renew$Eact)
g1 <- ggplot(energydataset1_renew, aes(energydataset1_renew$event_time))
g1 <- g1 + geom_line(aes(y=I), colour="blue")
g1 + labs(x = "TIME",y="I",title = "CURRENT")




monday = daysData(energydataset1_renew,"MON")
tuesday = daysData(energydataset1_renew,"TUE")
wednesday=daysData(energydataset1_renew,"WED")
thursday=daysData(energydataset1_renew,"THU")
friday=daysData(energydataset1_renew,"FRI")
saturday=daysData(energydataset1_renew,"SAT")
sunday=daysData(energydataset1_renew,"SUN")



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
