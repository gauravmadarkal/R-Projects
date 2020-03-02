#install.packages('coindeskr') 
#devtools::install_github("business-science/anomalize")
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
btc <- get_historic_price(start = "2017-01-01")
btc_ts <- btc %>% rownames_to_column() %>% as.tibble() %>% 
  mutate(date = as.Date(rowname)) %>% select(-one_of('rowname'))
btc_ts %>% 
  time_decompose(Price, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()
btc_ts %>% 
  time_decompose(Price) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)



sample %>%group_by(sample$daily_dates)%>%
  time_decompose(sample$daily_dates) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)


sample %>%
  time_decompose(sample$event_time, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()


install.packages("tibble")
# Loading
library("tibble")