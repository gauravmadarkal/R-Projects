library(caret)

library(dplyr)
quaddata=read.csv("C:\\Users\\SakshamA\\Documents\\Girish_Quad\\Quad_Datasets\\QUD5A84f3eb5304cc_set1.csv")

quaddata_renew=quaddata[,-c(15)]
quaddata_renew=na.omit(quaddata_renew)
colnames(quaddata_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
quaddata_renew=subset(quaddata_renew,Eact>=0)
quaddata_renew$event_time= strptime(quaddata_renew$event_time,"%Y-%m-%d %H:%M:%OS")
#to print only date then use ,as.Date(quaddata_renew$event_time, "%Y-%m-%d")
plot(quaddata_renew$event_time, quaddata_renew$switch,pch = 16,cex.axis = 0.5,col = "blue")
#lines(quaddata_renew$event_time,quaddata_renew$switch , col = "steelblue")
quaddata_renew$event_time=as.POSIXct(quaddata_renew$event_time)

quaddata_renew$date= as.POSIXct(quaddata_renew$event_time,format='%Y-%m-%d',tz= "UTC")
#quaddata_renew$date=as.Date(quaddata_renew$date)
time_count= summarise(group_by(quaddata_renew,as.Date(quaddata_renew$date),quaddata_renew$switch),count=n())
colnames(time_count)= c("date","switch","counting")

time_count=group_by(time_count,date ) %>% mutate(percent = counting/sum(counting))
filter_date=with(time_count , time_count[(date >= "2019-01-09"),])


##############################################################################

#timeline grAPH FOR date above Jan 9

quaddate_filter=with(quaddata_renew , quaddata_renew[(date >= "2019-01-09"),])
base <- ggplot(quaddate_filter, aes(event_time, switch),las=2,color="green") +geom_line(color="green")
base
#################################################################################

#timeline graph for one day

#oneday_frame=with(quaddata_renew , quaddata_renew[(date >= "2019-01-09" & date <"2019-01-10"),])
oneday_frame=with(quaddata_renew , quaddata_renew[(date >= "2019-01-12"),])
base <- ggplot(oneday_frame, aes(event_time, switch),las=2,color="green") +geom_line(color="green")
base
################################################################################

#pie chart for one day

pie(table(oneday_frame$switch), labels = paste(round(prop.table(table(oneday_frame$switch))*100), "%", sep = ""),col = c("RED","GREEN"), main ="Switch: ON/OFF")
legend("topright", legend = c("OFF","ON"),fill = c("RED","GREEN"), title = "SWITCHING", cex = 1.0)

##################################################################################

#plot of Voltage and Current in one plot

g <- ggplot(oneday_frame, aes(oneday_frame$event_time))
g <- g + geom_line(aes(y=V), colour="green")
g <- g + geom_line(aes(y=I), colour="red")
g + labs(x = "TIME",y="V,I",title = "VI plot")

##################################################################################

##to find power 

sum(quaddate_filter$Eact)  ##above jan9

sum(oneday_frame$Eact)  ##one day

###################################################################################

#based on time graph for one day

night<-g+ xlim(as.POSIXct(c('2019-01-12 00:00:44', '2019-01-12 06:02:18'), format="%Y-%m-%d %H:%M:%OS"))+ labs(x = "TIME",y="V,I",title = "NIGHT")
morning<-g+ xlim(as.POSIXct(c('2019-01-12 06:00:00', '2019-01-12 10:02:18'), format="%Y-%m-%d %H:%M:%OS"))+ labs(x = "TIME",y="V,I",title = "MORNING")
midmorning<- g+ xlim(as.POSIXct(c('2019-01-12 10:00:00', '2019-01-12 15:02:18'), format="%Y-%m-%d %H:%M:%OS"))+ labs(x = "TIME",y="V,I",title = "MIDMORNING")

###################################################################################
##time divisions in one graph
library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))

print(night, vp = vplayout(1,1))
print(morning, vp = vplayout(1,2))
print(midmorning, vp = vplayout(1,3))
######################################################################################

#moving means and centred moving means
library(zoo)
moving_means= as.data.frame(rollmean(oneday_frame$V,10))
colnames(moving_means)=c("means")
centred_moving_means=as.data.frame(rollmean(moving_means$means,2))
colnames(centred_moving_means)=c("means")

#####################################################################################

#forcasting the future data

power_ts=ts(oneday_frame$Eact,start=min(oneday_frame$event_time),frequency=60)
plot.ts(power_ts)

power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)

power_ts_hw=HoltWinters(power_ts,beta = F,gamma =F)#Holtwinters 
library(forecast)
#forecast

power_ts_hw_fcst=forecast:::forecast.HoltWinters(power_ts_hw,n.ahead=1)
forecast:::plot.forecast(power_ts_hw_fcst)



