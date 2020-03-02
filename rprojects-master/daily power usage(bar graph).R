homedata = read.csv("C:\\Users\\Sathishkumar.k\\Documents\\gauravProjects\\Quad_Datasets\\gaurav_data.csv")
homedata_renew=homedata[,-c(15)]
homedata_renew=na.omit(homedata_renew)
colnames(homedata_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
homedata_renew=subset(homedata_renew,Eact>=0)
homedata_renew$event_time= strptime(homedata_renew$event_time,"%Y-%m-%d %H:%M:%OS")

homedata_renew$event_time=as.POSIXct(homedata_renew$event_time)

homedata_renew$date= as.POSIXct(homedata_renew$event_time,format='%Y-%m-%d',tz= "UTC")

dayNumber=0
day1 = homedata_renew$EveryDate[1]
for (i in homedata_renew$EveryDate) {
  ifelse(homedata_renew$EveryDate == day1,dayNumber,day1 = homedata_renew$EveryDate)
}




daily_renew$num <- seq.int(nrow(daily_renew))
ggplot(data =homedata_renew, aes(x =event_time, y =Eact)) + geom_line(color = as.Date(homedata_renew$date))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data =daily_renew, aes(x =daily_renew$daily_dates, y =daily_renew$daily_power_usage)) + geom_bar(color = as.Date(daily_renew$daily_dates),stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = daily_renew$daily_dates) 