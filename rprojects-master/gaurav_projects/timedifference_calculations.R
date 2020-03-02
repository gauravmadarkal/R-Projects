homedata = read.csv("C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\gaurav_data.csv")
homedata_renew=homedata[,-c(15)]
homedata_renew=na.omit(homedata_renew)
colnames(homedata_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
homedata_renew=subset(homedata_renew,Eact>=0)
homedata_renew$event_time= strptime(homedata_renew$event_time,"%Y-%m-%d %H:%M:%OS")

homedata_renew$event_time=as.POSIXct(homedata_renew$event_time)

homedata_renew$date= as.POSIXct(homedata_renew$event_time,format='%Y-%m-%d',tz= "UTC")
homedata_renew = homedata_renew[order(homedata_renew$date,decreasing = FALSE),]
homedata_renew$EveryDate = as.Date(homedata_renew$date)
mydates = homedata_renew[c("event_time","EveryDate")]
mydates$EveryDate = strptime(mydates$event_time,"%Y-%m-%d")
mydates$EveryDate = as.Date(mydates$EveryDate)
library(dplyr)
mydates = mydates %>% 
  mutate(ID = group_indices_(mydates, .dots=c("EveryDate")))

mydates = mydates[order(mydates$ID,decreasing = FALSE),]


everydateDF = mydates[c("EveryDate","event_time")]



x = paste(everydateDF$EveryDate,"T00:00:00.500-08:00",sep = "") 
x = as.POSIXct(x,format = "%Y-%m-%dT%H:%M:%OS")
everydateDF$startTime = x
everydateDF = everydateDF[c(-2)]
everydateDF = unique(everydateDF)
everydateDF = everydateDF %>% 
  mutate(ID = group_indices_(everydateDF, .dots=c("EveryDate")))
colnames(everydateDF) = c("EveryDate","event_time","ID")
mydf = rbind(everydateDF,mydates)
mydf = mydf[order(mydf$ID,decreasing = FALSE),]

mydates = mydf

dates1 = mydates$event_time
#dates2 = dates1[2:length(dates1)]
#dates2[length(dates1)] = dates1[length(dates1)]

date1 = mydates$event_time[1]
dates2 = c(date1,mydates$event_time[1:length(mydates$event_time)-1])
mydates$timeDifference = difftime(dates2,dates1,units = "mins")
mydates$timeDifference = abs(mydates$timeDifference)
mydates$timeDifference = round(mydates$timeDifference)


mydates$tdiff <- unlist(tapply(mydates$event_time, INDEX = mydates$EveryDate,
                               FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
mydates$tdiff = round(mydates$tdiff)

gap_between_records = mydates[which(mydates$tdiff>5),]
gap_between_records$EveryDate = strptime(gap_between_records$event_time,"%Y-%m-%d")
gap_between_records$EveryDate = as.Date(gap_between_records$EveryDate)


offtimedata = aggregate(gap_between_records$tdiff,by = list(gap_between_records$EveryDate),sum)
colnames(offtimedata) = c("date","device_off_time")

offtimedata$device_on_time = 1440 - offtimedata$device_off_time 
library(ggplot2)
ggplot(offtimedata,aes(x = EveryDate,y = device_on_time)) + geom_bar(stat = "identity",position = "dodge",color = offtimedata$EveryDate) +geom_text(label =offtimedata$device_on_time, position = position_dodge(width = 0.9),vjust = -0.25) 




everydateDF$ID <- seq.int(nrow(everydateDF))
colnames(everydateDF) = c("EveryDate","event_time","ID")
mydf = merge(mydates,everydateDF,by = "ID",all.x=TRUE)

mydata = homedata_renew[c("EveryDate","switch","event_time")]
d = mydata %>% group_by(EveryDate) %>% mutate(switch_on = length(which(switch == 1)))

count_days = count(homedata_renew,EveryDate)

colnames(offtimedata) = c("EveryDate","device_off_time","device_on_time")

y = merge(offtimedata,count_days,by = "EveryDate",all = TRUE)

switch_on_data = d[c("EveryDate","switch_on")]
switch_on_data = unique(switch_on_data)

z = merge(offtimedata,switch_on_data,by = "EveryDate",all = TRUE)
y = merge(offtimedata,count_days,by = "EveryDate",all = TRUE)
switch_on_percentage_data = merge(y,z,by = "EveryDate",all = TRUE)
switch_on_percentage_data = switch_on_percentage_data[c(-5,-6)]
colnames(switch_on_percentage_data) = c("EveryDate","device_off","device_on","n","switch_on")

switch_on_percentage_data$active_percentage = (switch_on_percentage_data$n/1440) *100
switch_on_percentage_data$percentageSwitch_ON = (switch_on_percentage_data$switch_on)/switch_on_percentage_data$n * switch_on_percentage_data$active_percentage

switch_on_percentage_data$switch_off_percentage = switch_on_percentage_data$active_percentage - switch_on_percentage_data$percentageSwitch_ON

graph = ggplot(switch_on_percentage_data,aes(x=switch_on_percentage_data$EveryDate))+ geom_line(aes(y=switch_on_percentage_data$active_percentage,color = "active"))+ geom_line(aes(y=switch_on_percentage_data$percentageSwitch_ON,color ="switch on"))+geom_line(aes(y=switch_on_percentage_data$switch_off_percentage,color = "switch off"))+ theme(legend.position = c(0,1),legend.justification = c(0,1), axis.text.x = element_text(angle = 90,hjust = 1)) + scale_x_date(breaks = switch_on_percentage_data$EveryDate)+scale_colour_manual(values = c("red","blue","green"))


power_df_helper = homedata_renew[c("EveryDate","Eact")]
cumulative_power_df = aggregate(power_df_helper$Eact, by=list(Category=power_df_helper$EveryDate), FUN=sum)
colnames(cumulative_power_df) = c("EveryDate","Power_Usage")
ggp = ggplot(cumulative_power_df,aes(x= cumulative_power_df$EveryDate))+geom_line(aes(y=cumulative_power_df$Power_Usage),colour = "black",size = 1.5)+geom_rect(aes(xmin=cumulative_power_df$EveryDate[1],xmax=cumulative_power_df$EveryDate[49], ymin = 400, ymax = Inf),fill = "red", alpha = 0.01)+geom_rect(aes(xmin=cumulative_power_df$EveryDate[1],xmax=cumulative_power_df$EveryDate[49], ymin = 100, ymax = 400),fill = "green", alpha = 0.01)+geom_rect(aes(xmin=cumulative_power_df$EveryDate[1],xmax=cumulative_power_df$EveryDate[49], ymin = 0, ymax = 100),fill = "blue", alpha = 0.01)
ggp = ggp + annotate("text",x=cumulative_power_df$EveryDate[48],y = 600,label = "High,600")+ annotate("text",x=cumulative_power_df$EveryDate[48],y = 300,label = "Medium,300")+ annotate("text",x=cumulative_power_df$EveryDate[49],y = 50,label = "Low,50")+annotate("text",x=cumulative_power_df$EveryDate[49],y = cumulative_power_df$Power_Usage[49],label = "usage")
ggp = ggp + theme(axis.text.x = element_text(angle = 60,hjust = 1))
ggp = ggp +   scale_x_date(breaks = cumulative_power_df$EveryDate) 
pdf(file = "C:\\Users\\USER\\Documents\\project\\R files\\rprojects-master\\Quad_Datasets\\report.pdf")
print(ggp)
print(graph)
dev.off()
