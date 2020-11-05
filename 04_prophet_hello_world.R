library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(prophet)

options(scipen=999)

datos <- read_csv("Data_Scientist_Assessment_Data_ok.csv")

glimpse(datos)

ggplot(datos) +
  geom_line(aes(x = date, y = gmv, group = 1))

ggplot(datos) +
  geom_line(aes(x = date, y = ads_spend, group = 1))

#--------------------------------------------
# all defaults 

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(df)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# add fleaxibility to seasonality (watch out with overfitting)

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(df, changepoint.prior.scale = .2)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# add half year and monthly seasonality

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(yearly.seasonality=FALSE)
#m <- add_seasonality(m, name='biweekly', period=15.25, fourier.order=6)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=6) # menor mape con monthly que con bieekly
m <- add_seasonality(m, name='semiannual', period=182.5, fourier.order=7)
m <- fit.prophet(m, df)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# add holiday effects

df <- datos %>% select(ds = date, y = gmv)

holidays <- data_frame(
  holiday = 'dummy',
  ds = as.Date(c('2020-12-24')),
  lower_window = 0,
  upper_window = 1
) %>% filter(holiday != "dummy")

# fit the model
m <- prophet(holidays = holidays)
m <- add_country_holidays(m, country_name = 'MX')
m <- fit.prophet(m, df)

m$train.holiday.names

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# effects of holidays
forecast %>% 
  select(ds, Christmas) %>% 
  filter(abs(Christmas) > 0) %>%
  tail(10)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# check multiplicative effects

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(df, seasonality.mode = 'multiplicative')

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# add holiday effects and multiplicative effects

df <- datos %>% select(ds = date, y = gmv)

holidays <- data_frame(
  holiday = 'dummy',
  ds = as.Date(c('2020-12-24')),
  lower_window = 0,
  upper_window = 1
) %>% filter(holiday != "dummy")

# fit the model
m <- prophet(seasonality.mode = 'multiplicative', holidays = holidays)
m <- add_country_holidays(m, country_name = 'MX')
m <- fit.prophet(m, df)

m$train.holiday.names

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# check confidence intervals 70%

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(df, interval.width = 0.70)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)


#--------------------------------------------
# add mcmc, bayesian sampling to estimate intervals for seasonal effects

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(df, mcmc.samples = 300)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)



#--------------------------------------------
# cross validation

df <- datos %>% select(ds = date, y = gmv)

# fit the model
m <- prophet(df)

# validation
sum(df$ds<"2019-01-01") # validate since 2019
df.cv <- cross_validation(m, initial = 456, period = 182, horizon = 365, units = 'days')
head(df.cv)

ggplot(df.cv) +
  geom_point(aes(x = ds, y  = y), color = "black") +
  geom_line(aes(x = ds, y = yhat), color = "blue") +
  geom_line(aes(x = ds, y = yhat_lower), color = "blue", linetype = "dashed") +
  geom_line(aes(x = ds, y = yhat_upper), color = "blue", linetype = "dashed")

# performance metrics
df.p <- performance_metrics(df.cv)
head(df.p)

# plot errors
plot_cross_validation_metric(df.cv, metric = 'mape')


#--------------------------------------------
# adding a regressor

df <- datos %>% select(ds = date, y = gmv, ads_spend)

# fit the model
m <- prophet()
m <- add_regressor(m, 'ads_spend')
m <- fit.prophet(m, df)

# predict 
future <- make_future_dataframe(m, periods = 365)
future %<>% left_join(df %>% select(ds, ads_spend), by = "ds")
future$ads_spend[future$ds > "2020-07-21"] <- 6000
sum(is.na(future$ads_spend))
forecast <- predict(m, future)

# plot predictions
plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)

