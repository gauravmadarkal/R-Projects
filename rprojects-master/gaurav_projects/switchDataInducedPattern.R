inducedData = homedata_renew[which(homedata_renew$EveryDate == "2019-02-16"),]
write.table(homedata_renew,sep = "\t",file = "data.txt",row.names = FALSE)
d = read.table("data.txt",sep = "\t",header = TRUE)


intervals.2.min <- 0 : (50 * 24 * 60 * 60 / 2 / 60)
res <- as.POSIXct("2019-01-08","GMT") + intervals.2.min * 2 * 60
res <- res[res < as.POSIXct("2019-02-27 00:00:00 GMT")]
head(res)
dateVector = strptime(res,"%Y-%m-%d")
dateVector = as.Date(dateVector)
head(dateVector)

myDatesDF = as.data.frame(res)
myDatesDF$EveryDate = dateVector
colnames(myDatesDF) = c("event_time","EveryDate")
myDatesDF = myDatesDF[-c(35281:35835),]
indexColumn = c(1:720)
indexColumn = rep(indexColumn,times = 49)
myDatesDF$repeatID = indexColumn
morningTime = "2019-01-08 09:00:00"
morningTime = as.POSIXct(morningTime,"GMT")
eveningTime = "2019-01-08 18:00:00"
eveningTime = as.POSIXct(eveningTime,"GMT")
switchValue = ifelse(myDatesDF$event_time >  morningTime & myDatesDF$event_time < eveningTime,1,0)
switchValue = switchValue[c(1:720)]
switchValue = rep(switchValue,times = 49)
myDatesDF$Switch = switchValue
write.table(myDatesDF,sep = "\t",file = "InducedPattern.txt",row.names = FALSE)
library(ggplot2)
g10 <- ggplot(myDatesDF, aes(myDatesDF$event_time))
g10 <- g10 + geom_line(aes(y=myDatesDF$Switch), colour="blue")
g10 + labs(x = "TIME",y="SWITCH",title = "TIME VS SWITCH plot")

diffswitch = diff(myDatesDF$Switch,1)
adf.test(myDatesDF$Switch)
adf.test(diffswitch)

library(tseries)
library(forecast)
dt = "2019-02-26 00:02:00"
dt = as.POSIXct(dt,"GMT")
switch_ts = ts(myDatesDF$Switch,start = dt,frequency = 720)
fit = auto.arima(switch_ts)

voltage_ts = ts(homedata_renew$V,frequency = 7)


forc = forecast(fit,h=720)
predictions = as.data.frame(forc)
intervals.2.min <- 0 : (24 * 60 * 60 / 2 / 60)
res <- as.POSIXct("2019-02-26","GMT") + intervals.2.min * 2 * 60
res <- res[res < as.POSIXct("2019-02-28 00:00:00 GMT")]
res = res[1:720]
predictions = as.data.frame(predictions$`Point Forecast`)
predictions$event_time = res
colnames(predictions) = c("switch","event_time")
plot(predictions$switch)
library(ggplot2)
g10 <- ggplot(predictions, aes(predictions$event_time))
g10 <- g10 + geom_line(aes(y=predictions$switch), colour="blue")
g10 + labs(x = "TIME",y="SWITCH",title = "TIME VS SWITCH plot")
switch_ts_decomp=decompose(switch_ts)#to get random,observed ,seasonal data
plot(switch_ts_decomp)

voltage_ts_decomp = decompose(voltage_ts)
plot(voltage_ts_decomp)

pattern.on.DF = predictions[which(predictions$switch == 1),]
patternONData = pattern.on.DF[c(1,length(pattern.on.DF[,1])),]
library(jsonlite)
library(readr)
patternONData %>% 
  toJSON() %>% 
  write_lines("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\PredictedData.json")

