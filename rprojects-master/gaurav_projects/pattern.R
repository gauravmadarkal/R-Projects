myDatesDF$DayNames =format(myDatesDF$event_time,"%a")
table(myDatesDF$DayNames)

trailData = myDatesDF
WeekdayOnTime1 = "2019-01-08 12:00:00"
WeekdayOnTime1 = as.POSIXct(WeekdayOnTime1,"GMT")
WeekdayOffTime1 = "2019-01-08 16:00:00"
WeekdayOffTime1 = as.POSIXct(WeekdayOffTime1,"GMT")

WeekdayOnTime2 = "2019-01-08 00:00:00"
WeekdayOnTime2 = as.POSIXct(WeekdayOnTime2,"GMT")
WeekdayOffTime2 = "2019-01-08 06:00:00"
WeekdayOffTime2 = as.POSIXct(WeekdayOffTime2,"GMT")

WeekdayOnTime1
WeekdayOnTime2
WeekdayOffTime1
WeekdayOffTime2
switchValue = rep(0,times= 720)

switchValue = ifelse(trailData$event_time >=  WeekdayOnTime1 & trailData$event_time < WeekdayOffTime1 ,1,0)
switchValue = ifelse(trailData$event_time >=WeekdayOnTime2 & trailData$event_time < WeekdayOffTime2,1,switchValue)

switchValue = switchValue[c(1:720)]

OneDayDates = trailData$event_time[c(1:720)]

testDf = as.data.frame(OneDayDates)
testDf$switch = switchValue
values = rep(switchValue,times = 49)
trailData$Switch = switchValue

timedata = format(as.POSIXct(strptime(trailData$event_time,"%Y-%m-%d %H:%M:%OS",tz="")),format = "%H:%M")

trailData$Time = timedata

library(ggplot2)
g10 <- ggplot(trailData, aes(trailData$event_time))
g10 <- g10 + geom_line(aes(y=trailData$Switch), colour="blue")
g10 + labs(x = "TIME",y="SWITCH",title = "TIME VS SWITCH plot")

trailData$Switch = ifelse(trailData$hour == 22 | trailData$hour == 23 | trailData$hour >= 0 & trailData$hour <= 6 | trailData$hour>=12 & trailData$hour<=16,1,0)

inducedPowerData = read.csv("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\power_data.csv")  
colnames(inducedPowerData) = c("V","Eact","Device")


voltageList = rep(inducedPowerData$V,times=49)
powerList = rep(inducedPowerData$Eact,times=49)
deviceList = rep(inducedPowerData$Device,times = 49)

trailData$V = voltageList
trailData$Eact = powerList
trailData$Device = deviceList

trailData$Eact = ifelse(trailData$Switch == 1,trailData$Eact,0)
trailData$Eact = ifelse(trailData$Eact < 0,sum(trailData$Eact)/length(which(trailData$Eact != 0)),trailData$Eact)

library(ggplot2)
g10 <- ggplot(trailData, aes(trailData$event_time))
g10 <- g10 + geom_line(aes(y=trailData$Eact), colour="blue")
g10 + labs(x = "TIME",y="SWITCH",title = "TIME VS SWITCH plot")

library(tseries)
library(forecast)
power_ts = ts(trailData$Eact,frequency = 720)
fit = auto.arima(power_ts)
forcs = forecast(fit, h= 720)
plot(forcs)
#write.table(myDatesDF,sep = "\t",file = "powerInduceddata.txt",row.names = FALSE)

switch_ts = ts(trailData$Switch,frequency = 720)
fitswitch = auto.arima(switch_ts)
forcsswitch = forecast(fitswitch, h= 720)
plot(forcsswitch)
