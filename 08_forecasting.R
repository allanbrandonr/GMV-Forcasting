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

# trend
# changepoint.range: percentaje of data up to wich changepoints are considered
#     we may include first months of 2020 which have a change in trend bc covid
# n_changepoints: number of changepoints to try
# changepoints: specific changepoints if wanted
# changepoint.prior.scale: flexibility parameter, increasing may cause overfitting 
#     in history but produce wide uncertainty intervlas

# seasonality
# seasonality.mode: aditive or multiplicative
# fourier.order: number of sums to consider in the fourier series
#     increasing it allows to fit seasonal patterns that change more quickly
# holidays: list of holidays

# some paramters
set.seed(28)
holidays <- read_csv("holidays.csv")

# fit the model
m <- prophet(# trend
             changepoint.range = 1,
             n.changepoints = 50,
             # seasonality
             seasonality.mode = 'multiplicative',
             yearly.seasonality = FALSE,
             weekly.seasonality = 3,
             # holidays
             holidays = holidays,
             holidays.prior.scale = .8)
m <- add_seasonality(m, name = 'monthly', period = 30.5, fourier.order = 11)
#m <- add_seasonality(m, name = 'semiannualy', period = 182.5, fourier.order = 3)
m <- fit.prophet(m, df)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# cross validation
df.cv <- cross_validation(m, initial = which(df$ds == "2019-06-29"), period = 182, horizon = 365, units = 'days')
df.p <- performance_metrics(df.cv, rolling_window = 30/365)
plot_cross_validation_metric(df.cv, metric = 'mape', rolling_window = 30/365)

df.cv <- cross_validation(m, initial = which(df$ds == "2019-12-30"), period = 182, horizon = 182, units = 'days')
df.p <- performance_metrics(df.cv, rolling_window = 30/182)
plot_cross_validation_metric(df.cv, metric = 'mape', rolling_window = 30/182)


# plot predictions
plot(m, forecast)
plot(m, forecast) + coord_cartesian(ylim = quantile(df$y, c(0, 0.99), na.rm = T))
plot(m, forecast) + add_changepoints_to_plot(m)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)
