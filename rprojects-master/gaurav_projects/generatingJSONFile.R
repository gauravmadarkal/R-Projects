library(readr)
library(jsonlite)


sunrise = paste(mydates$EveryDate," 07:00:00")
sunrise = as.POSIXct(sunrise)
mydates$sunrise = sunrise

sunset = paste(mydates$EveryDate," 22:00:00")
mydates$sunset = sunset

mydates$sunset = as.POSIXct(mydates$sunset)
mydates$event_time[1] > mydates$sunrise[1]
mydates$event_time[1] > mydates$sunset[1]
mydates$daytime = ifelse(mydates$event_time >= mydates$sunrise & mydates$event_time <= mydates$sunset,"True","False")

mydates$gapFound_in_daytime = ifelse(mydates$daytime == "True" & mydates$timeDifference > 300,"GAP","NAG")
df= mydates[which(mydates$gapFound_in_daytime == "GAP"),]
df= df[c(1,2,4,8,9)]
df %>% 
  toJSON() %>% 
  write_lines("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\data.json")


myDatesDF$percentageVariation = (100 - ((myDatesDF$V/230) * 100))
myDatesDF$percentageVariation = abs(myDatesDF$percentageVariation)
myDatesDF$categoryVariation = ifelse(myDatesDF$percentageVariation < 5 ,0 , ifelse(myDatesDF$percentageVariation >= 5 & myDatesDF$percentageVariation < 8, 5,ifelse(myDatesDF$percentageVariation >= 8 & myDatesDF$percentageVariation < 10,8,ifelse(myDatesDF$percentageVariation >= 10 & myDatesDF$percentageVariation < 12, 10,12))))
myDatesDF$limitcrossed=ifelse(myDatesDF$V < 240 & myDatesDF$V > 200 ,"NO DANGER","DANGER")

library(ggplot2)
g1 <- ggplot(myDatesDF, aes(myDatesDF$event_time,group=myDatesDF$limitcrossed,col=limitcrossed))
g1 <- g1 + geom_point(aes(y=myDatesDF$V))
g1 + labs(x = "days",y="voltage variation",title = "daysname vs voltage variation")


voltageVector = homedata_renew$V
indices = c(31517:35280)
voltageVector[indices] = sum(voltageVector) / length(voltageVector)
myDatesDF$V = voltageVector


voltageAnamoliesDF = c("No DANGER","DANGER") 
voltageAnamoliesDF = as.data.frame(voltageAnamoliesDF)
voltageAnamoliesDF$proportion = c(length(which(myDatesDF$limitcrossed == "NO DANGER")) / length(myDatesDF$limitcrossed),length(which(myDatesDF$limitcrossed == "DANGER")) / length(myDatesDF$limitcrossed))
colnames(voltageAnamoliesDF) = c("category","proportion")

voltageAnamoliesDF$proportion = voltageAnamoliesDF$proportion *100
library(jsonlite)
library(readr)
library(dplyr)
voltageAnamoliesDF %>% 
  toJSON() %>% 
  write_lines("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\Voltagedata.json")

