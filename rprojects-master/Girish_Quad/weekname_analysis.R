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
energydataset1=read.csv("C:\\Users\\SakshamA\\Documents\\Girish_Quad\\Quad_Datasets\\gaurav_data.csv")
energydataset1_renew=energydataset1[,-c(15)]
energydataset1_renew=na.omit(energydataset1_renew)
colnames(energydataset1_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
energydataset1_renew=subset(energydataset1_renew,Eact>=0)
energydataset1_renew$event_time= strptime(energydataset1_renew$event_time,"%Y-%m-%d %H:%M:%OS")
#to print only date then use ,as.Date(energydataset1_renew$event_time, "%Y-%m-%d")
plot(energydataset1_renew$event_time, energydataset1_renew$switch,pch = 16,cex.axis = 0.5,col = "blue")
#lines(energydataset1_renew$event_time,energydataset1_renew$switch , col = "steelblue")
energydataset1_renew$event_time=as.POSIXct(energydataset1_renew$event_time)

energydataset1_renew$date= as.POSIXct(energydataset1_renew$event_time,format='%Y-%m-%d',tz= "UTC")

energydataset1_renew$daynames=format(energydataset1_renew$date,"%a")
energydataset1_renew=energydataset1_renew[ order(energydataset1_renew$event_time , decreasing = FALSE ),]
energydataset1_renew$Eact[which(energydataset1_renew$switch ==0 & energydataset1_renew$Eact > 0)] = 0


#day = daysData(energydataset1_renew,"WEEKDAY")
"print(day)


power_ts=ts(day$Eact,start=min(day$event_time),frequency=60)
plot.ts(power_ts)

power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)

power_ts_hw=HoltWinters(power_ts,beta = F,gamma =F)#Holtwinters 
library(forecast)
#forecast

power_ts_hw_fcst=forecast:::forecast.HoltWinters(power_ts_hw,n.ahead=1)
forecast:::plot.forecast(power_ts_hw_fcst)
"
weekday = daysData(quaddata_renew,"WEEKDAY")
weekend = daysData(quaddata_renew,"WEEKEND")

g <- ggplot(weekday, aes(weekday$event_time))
g <- g + geom_line(aes(y=V), colour="green")
g <- g + geom_line(aes(y=I), colour="red")
g + labs(x = "TIME",y="V,I",title = "VI plot")
#################################################################################
 #weekend and weekdays overlay graph


weekday = daysData(quaddata_renew,"WEEKDAY")
weekend = daysData(quaddata_renew,"WEEKEND")
combinedPlot = rbind(weekday,weekend)
combinedPlot$extraCol = c(rep("weekdays",18402),rep("weekend",8583))

g1 <- ggplot(combinedPlot, aes(combinedPlot$event_time,group=combinedPlot$extraCol,col=combinedPlot$extraCol))
g1 <- g1 + geom_line(aes(y=V))
g1 <- g1 + geom_line(aes(y=I))

g1 + labs(x = "TIME",y="V,I",title = "VI plot")


################################################################################

#multiple graphs in one graph
g <- ggplot(weekday, aes(weekday$event_time))
#g <- g + geom_line(aes(y=V), colour="green")
#g <- g + geom_line(aes(y=I), colour="red")
g <- g + geom_line(aes(y=Eact), colour="blue")
g + labs(x = "TIME",y="V,I",title = "VI plot")
g+ylim(0,2)

g1 <- ggplot(weekend, aes(weekend$event_time))
#g1 <- g1 + geom_line(aes(y=V), colour="green")
#g1 <- g1 + geom_line(aes(y=I), colour="red")
g1 <- g1 + geom_line(aes(y=Eact), colour="blue")
g1 + labs(x = "TIME",y="V,I",title = "VI plot")
g1+ylim(0,2)


figure <- ggarrange(g, g1,labels = c("WEEKDAY", "WEEKEND"),ncol = 2, nrow = 1)
figure

###################################################################################

#cummulative power plot

energydataset1_renew=energydataset1_renew[ order(energydataset1_renew$event_time , decreasing = FALSE ),]
energydataset1_renew$cummulative_sum=cumsum(dummy_energy$Eact)
g1 <- ggplot(energydataset1_renew, aes(energydataset1_renew$event_time))
g1 <- g1 + geom_line(aes(y=cummulative_sum), colour="blue")
g1 + labs(x = "TIME",y="ENERGY",title = "CUMMULATIVE ENERGY plot")


#####################################################################################


