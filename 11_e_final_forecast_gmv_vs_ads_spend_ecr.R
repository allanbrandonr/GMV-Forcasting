library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(prophet)
library(doParallel)

options(scipen=999)

source("09_forecasting_function.R")

#------------------------------------------------------------------------------

# format data for prophet functions
datos <- read_csv("Data_Scientist_Assessment_Data_ok.csv")
df <- datos %>% select(ds = date, y = gmv)
holidays <- read_csv("holidays.csv")
regressors <- list("ads_spend" = datos$ads_spend,
                   "effective_conversion_rate" = datos$effective_conversion_rate)

forecast_ads_spend <- read_csv("11_final_forecasts/forecast_ads_spend.csv") %>% select(ds, ads_spend = yhat)
forecast_ecr <- read_csv("11_final_forecasts/forecast_ecr.csv") %>% select(ds, effective_conversion_rate = yhat)
regressors.forecast <- list("ads_spend" = forecast_ads_spend,
                            "effective_conversion_rate" = forecast_ecr)

# parameter combinations to check 
d_grid <- read.csv("10_optimize_forecast/all_forecasts_gmv_vs_ads_spend_ecr.csv") %>% arrange(mape_20h1_30)
View(d_grid)
i <- 80

# make forecasts
pronos <- f_estima_pronosticos(dff = df, 
                               p.changepoint.range = d_grid$p.changepoint.range[i],
                               p.changepoint.prior.scale = d_grid$p.changepoint.prior.scale[i],
                               p.seasonality.mode = d_grid$p.seasonality.mode[i],
                               p.yearly.seasonality = d_grid$p.yearly.seasonality[i],
                               p.semiyearly.seasonality = d_grid$p.semiyearly.seasonality[i],
                               p.monthly.seasonality = d_grid$p.monthly.seasonality[i],
                               p.biweekly.seasonality = d_grid$p.biweekly.seasonality[i],
                               p.weekly.seasonality = d_grid$p.weekly.seasonality[i],
                               p.holidays.prior.scale = d_grid$p.holidays.prior.scale[i],
                               p.holidays = holidays,
                               p.regressors = regressors,
                               p.periods = 163,
                               p.regressors.forecast = regressors.forecast)

# review forecasts
plot(pronos$m, pronos$forecast)
plot(pronos$m, pronos$forecast) + coord_cartesian(ylim = quantile(df$y, c(0, 0.99), na.rm = T))
dyplot.prophet(pronos$m, pronos$forecast)

# review changepoints
plot(pronos$m, pronos$forecast) + add_changepoints_to_plot(pronos$m) 

# review components
prophet_plot_components(pronos$m, pronos$forecast)
 

#------------------------------------------------------------------------------

# save
write.csv(pronos$forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], "11_final_forecasts/forecast_gmv_vs_ads_spend_ecr.csv", row.names = F)

p <- plot(pronos$m, pronos$forecast) + add_changepoints_to_plot(pronos$m) + coord_cartesian(ylim = c(-25000,250000))
png(paste0("11_final_forecasts/changepoints_gmv_vs_ads_spend_ecr.png"), width = 800, height = 500)
print(p)
dev.off()

png(paste0("11_final_forecasts/components_gmv_vs_ads_spend_ecr.png"), width = 800, height = 500)
prophet_plot_components(pronos$m, pronos$forecast) 
dev.off()