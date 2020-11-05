library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(prophet)

options(scipen=999)

# glimpse(datos)
# ggplot(datos) + geom_line(aes(x = date, y = gmv, group = 1))
# ggplot(datos) + geom_line(aes(x = date, y = ads_spend, group = 1))


#------------------------------------------------------------------------------

# format data for prophet functions
datos <- read_csv("Data_Scientist_Assessment_Data_ok.csv")
df <- datos %>% select(ds = date, y = gmv)

dir.create("06_trend")

# some paramters
for(n in seq(10,400,20)){
  print(n)
  set.seed(28)
  prop_cp <- df$ds[seq(1, nrow(df), length.out = n) %>% floor()]
  holidays <- read_csv("holidays.csv")
  
  # fit the model
  m <- prophet(# trend
    changepoints = prop_cp,
    # seasonality
    seasonality.mode = 'multiplicative',
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    # holidays
    holidays = holidays,
    holidays.prior.scale = .8)
  m <- fit.prophet(m, df) 
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  p <- plot(m, forecast) + add_changepoints_to_plot(m) 
  png(paste0("06_trend/n_changepoints_",n,".png"), width = 800, height = 500)
  print(p)
  dev.off()
}


