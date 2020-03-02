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

library(forecast)
library(ggplot2)
g <- ggplot(energydataset1_renew, aes(energydataset1_renew$event_time))
g <- g + geom_line(aes(y=I), colour="blue")
g + labs(x = "TIME",y="CURRENT",title = "CURRENT VS TIME plot")
g <- ggplot(energydataset1_renew, aes(energydataset1_renew$event_time))
g <- g + geom_line(aes(y=I), colour="red")
g + labs(x = "TIME",y="CURRENT",title = "CURRENT VS TIME plot")
current_ts=ts(energydataset1_renew$I,start=min(energydataset1_renew$event_time),frequency=24*60)
plot.ts(current_ts)
ses(current_ts, h=3, alpha=0.1, initial="simple")

smooth_current_ts=ma(current_ts,24*60)
plot(smooth_current_ts)
ses(smooth_current_ts, h=3, alpha=0.1, initial="simple")#gives null values(error)
ets(current_ts)
tbatsFit <- tbats(current_ts, use.parallel=TRUE, num.cores = 2) # fit tbats model
plot(forecast(tbatsFit)) # plot
components <- tbats.components(tbatsFit)
plot(components)
accuracy(tbatsFit)#error values