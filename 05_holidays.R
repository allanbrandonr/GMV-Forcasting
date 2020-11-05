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

dir.create("05_holidays")

# some paramters
#for(n in c(seq(.1,1,.1),seq(2,30,2))){
for(n in seq(.1,1,.1)){
  print(n)
  set.seed(28)
  holidays <- read_csv("holidays.csv")
  
  # fit the model
  m <- prophet(# trend
    # seasonality
    seasonality.mode = 'multiplicative',
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    # holidays
    holidays = holidays,
    holidays.prior.scale = n)
  m <- fit.prophet(m, df) 
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  p <- plot(m, forecast) + add_changepoints_to_plot(m) 
  png(paste0("05_holidays/scale_",n,".png"), width = 800, height = 500)
  print(p)
  dev.off()
}


