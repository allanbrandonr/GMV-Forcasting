library(ggplot2)
library(dplyr)
library(magrittr)

# Prohet page: https://facebook.github.io/prophet/
# Getting started: https://facebook.github.io/prophet/docs/quick_start.html#r-api
# CSV: https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv

# install from source
# install.packages("prophet", type="source")

library(prophet)

# read data
df <- read.csv('https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv')
df$ds %<>% as.Date()
glimpse(df)

# plot data
ggplot(df) +
  geom_line(aes(x = ds, y = y, group = 1))

# fit the model
m <- prophet(df)

# predict 
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# plot predictions
ggplot(forecast) +
  geom_line(aes(x = ds, y = yhat, group = 1))

plot(m, forecast)

# plot components
prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)


