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
regressors <- list("ads_spend" = datos$ads_spend)

# parameter combinations to check 

# yearly, monthly and weekly seasonalities
# d_grid <- expand.grid(p.changepoint.range = 1,
#                       p.changepoint.prior.scale = c(.05, .3, .6),
#                       p.seasonality.mode = c("additive", "multiplicative"),
#                       p.yearly.seasonality = seq(0, 12, 3),
#                       p.semiyearly.seasonality = 0,
#                       p.monthly.seasonality = seq(0, 12, 3),
#                       p.biweekly.seasonality = 0,
#                       p.weekly.seasonality = 3,
#                       p.holidays.prior.scale = c(.001, .8, 10))

# semiyearly, biweekly and weekly seasonalities
d_grid <- expand.grid(p.changepoint.range = 1,
                      p.changepoint.prior.scale = c(.05, .3, .6),
                      p.seasonality.mode = c("additive", "multiplicative"),
                      p.yearly.seasonality = 0,
                      p.semiyearly.seasonality = seq(0, 12, 3),
                      p.monthly.seasonality = 0,
                      p.biweekly.seasonality = seq(0, 12, 3),
                      p.weekly.seasonality = 3,
                      p.holidays.prior.scale = c(.001, .8, 10))

# specifics for parallel run
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  


# check all models
t1 <- Sys.time()
all_forecasts <- foreach(i = 1:nrow(d_grid),  .combine = "rbind") %dopar% {
  f_estima_modelo(dff = df,
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
                  p.regressors = regressors)

}
t2 <- Sys.time()
difftime(t2, t1, units = "min")

# save
# write.csv(all_forecasts, "10_optimize_forecast/all_forecasts_gmv_vs_ads_spend_1.csv", row.names = F)
write.csv(all_forecasts, "10_optimize_forecast/all_forecasts_gmv_vs_ads_spend_2.csv", row.names = F)


# append sets
all_forecasts_1 <- read_csv("10_optimize_forecast/all_forecasts_gmv_vs_ads_spend_1.csv")
all_forecasts_2 <- read_csv("10_optimize_forecast/all_forecasts_gmv_vs_ads_spend_2.csv")

all_forecasts <- rbind(all_forecasts_1, all_forecasts_2)
write.csv(all_forecasts, "10_optimize_forecast/all_forecasts_gmv_vs_ads_spend.csv", row.names = F)
