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
day = daysData(quaddata_renew,"WEEKDAY")
print(day)