library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(prophet)
library(doParallel)

options(scipen=999)


#------------------------------------------------------------------------------

# format data for prophet functions
datos <- read_csv("Data_Scientist_Assessment_Data_ok.csv")
df <- datos %>% select(ds = date, y = effective_conversion_rate)
holidays <- read_csv("holidays.csv")

# make forecasts
m <- prophet(df, seasonality.mode = 'multiplicative', holidays = holidays)

# predict 
future <- make_future_dataframe(m, periods = 163)
forecast <- predict(m, future)

# cross validation
# df.cv <- cross_validation(m, initial = which(df$ds == "2019-06-29"), period = 182, horizon = 365, units = 'days')
# df.p <- performance_metrics(df.cv, rolling_window = 30/365)
# df.p[1,]
# 
# df.cv <- cross_validation(m, initial = which(df$ds == "2019-12-30"), period = 182, horizon = 182, units = 'days')
# df.p <- performance_metrics(df.cv, rolling_window = 30/182)
# df.p[1,]

# review forecasts
plot(m, forecast)
plot(m, forecast) + coord_cartesian(ylim = quantile(df$y, c(0, 0.99), na.rm = T))
dyplot.prophet(m, forecast)

# review changepoints
plot(m, forecast) + add_changepoints_to_plot(m)

# review components
prophet_plot_components(m, forecast)


#------------------------------------------------------------------------------

# save
write.csv(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], "11_final_forecasts/forecast_ecr.csv", row.names = F)

p <- plot(m, forecast) + add_changepoints_to_plot(m) 
png(paste0("11_final_forecasts/changepoints_ecr.png"), width = 800, height = 500)
print(p)
dev.off()

png(paste0("11_final_forecasts/components_ecr.png"), width = 800, height = 500)
prophet_plot_components(m, forecast) 
dev.off()