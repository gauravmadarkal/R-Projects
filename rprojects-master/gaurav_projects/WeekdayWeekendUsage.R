GraphDaysData <- function(dataset,dayname){
  
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
weekdaysDataset = GraphDaysData(quaddata_renew,"WEEKDAY")
weekendDataset = GraphDaysData(quaddata_renew,"WEEKEND")



combinedPlot = rbind(weekdaysDataset,weekendDataset)
combinedPlot$extraCol = c(rep("weekdays",18402),rep("weekend",8583))

g1 <- ggplot(combinedPlot, aes(combinedPlot$event_time,group=combinedPlot$extraCol,col=combinedPlot$extraCol))
g1 <- g1 + geom_line(aes(y=V))
g1 <- g1 + geom_line(aes(y=I))
g1 + labs(x = "TIME",y="V,I",title = "VI plot")







