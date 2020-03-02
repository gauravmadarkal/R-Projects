library(ggpubr)
quaddata_renew$percentageVariation = (100 - ((quaddata_renew$V/230) * 100))
quaddata_renew$percentageVariation = abs(quaddata_renew$percentageVariation)
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

quaddata_renew$categoryVariation = ifelse(quaddata_renew$percentageVariation < 5 ,0 , ifelse(quaddata_renew$percentageVariation >= 5 & quaddata_renew$percentageVariation < 8, 5,ifelse(quaddata_renew$percentageVariation >= 8 & quaddata_renew$percentageVariation < 10,8,ifelse(quaddata_renew$percentageVariation >= 10 & quaddata_renew$percentageVariation < 12, 10,12))))
weekday=daysData(quaddata_renew,"WEEKDAY")
weekend=daysData(quaddata_renew,"WEEKEND")

g1 <- ggplot(weekend, aes(weekend$event_time))
g1 <- g1 + geom_point(aes(y=weekend$categoryVariation),color = "blue")
g1 + labs(x = "days",y="variation",title = "daysname vs category variation")

g <- ggplot(weekday, aes(weekday$event_time))
g <- g + geom_point(aes(y=categoryVariation),color="red")
g + labs(x = "days",y="variation",title = "daysname vs category variation")

figure=ggarrange(g1,g,labels=c("weekend","weekday"),ncol=2,nrow=1)
figure