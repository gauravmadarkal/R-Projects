myDatesDF$DayNames =format(myDatesDF$event_time,"%a")
table(myDatesDF$DayNames)

WeekdaymorningTime = "2019-01-08 09:00:00"
WeekdaymorningTime = as.POSIXct(WeekdaymorningTime,"GMT")
WeekdayeveningTime = "2019-01-08 18:00:00"
WeekdayeveningTime = as.POSIXct(WeekdayeveningTime,"GMT")

weekEndmorningTime = "2019-01-08 09:00:00"
weekEndmorningTime = as.POSIXct(weekEndmorningTime,"GMT")
weekEndeveningTime = "2019-01-08 21:00:00"
weekEndeveningTime = as.POSIXct(weekEndeveningTime,"GMT")

switchValue = ifelse(myDatesDF$event_time >  WeekdaymorningTime & myDatesDF$event_time < WeekdayeveningTime,1,0)
switchValue = switchValue[c(1:720)]


values = rep(switchValue,times = 5)




weekEndswitchValue = ifelse(myDatesDF$event_time >  weekEndmorningTime & myDatesDF$event_time < weekEndeveningTime,1,0)
weekEndswitchValue = weekEndswitchValue[c(1:720)]

weekendvalues = rep(weekEndswitchValue,times = 2)

OneWeekValues = c(values,weekendvalues)


fullweekswitchData = rep(OneWeekValues,times=7)


fullweekswitchData = fullweekswitchData[-c(1:720)]

fullweekswitchData = c(fullweekswitchData,switchValue)

myDatesDF$Switch = fullweekswitchData

inducedPowerData = read.csv("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\power_data.csv")  
colnames(inducedPowerData) = c("V","Eact","Device")


voltageList = rep(inducedPowerData$V,times=49)
powerList = rep(inducedPowerData$Eact,times=49)
deviceList = rep(inducedPowerData$Device,times = 49)

myDatesDF$V = voltageList
myDatesDF$Eact = powerList
myDatesDF$Device = deviceList

myDatesDF$Eact = ifelse(myDatesDF$Eact<0,0.4,myDatesDF$Eact)
myDatesDF$Eact = ifelse(myDatesDF$Switch == 0,0,myDatesDF$Eact)



library(ggplot2)
g10 <- ggplot(myDatesDF, aes(myDatesDF$event_time))
g10 <- g10 + geom_line(aes(y=myDatesDF$Eact), colour="blue")
g10 + labs(x = "TIME",y="SWITCH",title = "TIME VS SWITCH plot")


library(ggplot2)
g10 <- ggplot(myDatesDF, aes(myDatesDF$event_time))
g10 <- g10 + geom_line(aes(y=myDatesDF$Switch), colour="blue")
g10 + labs(x = "TIME",y="SWITCH",title = "TIME VS SWITCH plot")

library(tseries)
library(forecast)
power_ts = ts(myDatesDF$Eact,frequency = 720)
fit = arima(power_ts,order = c(1,0,720))
forcs = forecast(fit, h= 720)
plot(forcs)
#write.table(myDatesDF,sep = "\t",file = "powerInduceddata.txt",row.names = FALSE)

switch_ts = ts(myDatesDF$Switch,frequency = 720)
fitswitch = arima(switch_ts,order = c(1,0,1))
forcsswitch = forecast(fitswitch, h= 720)
plot(forcsswitch)
