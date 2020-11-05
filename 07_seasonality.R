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

dir.create("07_seasonality")

# some paramters
prop_cp <- df$ds[seq(1, nrow(df), length.out = 50) %>% floor()]
holidays <- read_csv("holidays.csv")


#------------------------------------------------------------------------------
# weekly

print("weekly")
dir.create("07_seasonality/weekly")

for (i in 2:15){
  print(i)
  # fit the model
  m <- prophet(# trend
               changepoints = prop_cp,
               # seasonality
               seasonality.mode = 'multiplicative',
               yearly.seasonality = FALSE,
               weekly.seasonality = i,
               # holidays
               holidays = holidays,
               holidays.prior.scale = .8)
  m <- fit.prophet(m, df)
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  png(paste0("07_seasonality/weekly/fourier_",i,".png"), width = 800, height = 500)
  prophet_plot_components(m, forecast)
  dev.off()

}

#------------------------------------------------------------------------------
# yearly

print("yearly")
dir.create("07_seasonality/yearly")

for (i in 2:15){
  print(i)
  # fit the model
  m <- prophet(# trend
    changepoints = prop_cp,
    # seasonality
    seasonality.mode = 'multiplicative',
    yearly.seasonality = i,
    weekly.seasonality = FALSE,
    # holidays
    holidays = holidays,
    holidays.prior.scale = .8)
  m <- fit.prophet(m, df)
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  png(paste0("07_seasonality/yearly/fourier_",i,".png"), width = 800, height = 500)
  prophet_plot_components(m, forecast)
  dev.off()
  
}

#------------------------------------------------------------------------------
# semiannualy

print("semiannualy")
dir.create("07_seasonality/semiannualy")

for (i in 2:15){
  print(i)
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
  m <- add_seasonality(m, name='semiannualy', period = 182.5, fourier.order=i)
  m <- fit.prophet(m, df)
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  png(paste0("07_seasonality/semiannualy/fourier_",i,".png"), width = 800, height = 500)
  prophet_plot_components(m, forecast)
  dev.off()
  
}

#------------------------------------------------------------------------------
# monthly

print("monthly")
dir.create("07_seasonality/monthly")

for (i in 2:15){
  print(i)
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
  m <- add_seasonality(m, name='monthly', period = 30.5, fourier.order=i)
  m <- fit.prophet(m, df)
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  png(paste0("07_seasonality/monthly/fourier_",i,".png"), width = 800, height = 500)
  prophet_plot_components(m, forecast)
  dev.off()
  
}

#------------------------------------------------------------------------------
# biweekly

print("biweekly")
dir.create("07_seasonality/biweekly")

for (i in 2:15){
  print(i)
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
  m <- add_seasonality(m, name='biweekly', period = 15.25, fourier.order=i)
  m <- fit.prophet(m, df)
  
  # predict 
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # plot predictions
  png(paste0("07_seasonality/biweekly/fourier_",i,".png"), width = 800, height = 500)
  prophet_plot_components(m, forecast)
  dev.off()
  
}




# by inspeciton best options are
# weekly 3
# yearly 6
# semiannualy 3
# monthly 11
# biweekly 7


