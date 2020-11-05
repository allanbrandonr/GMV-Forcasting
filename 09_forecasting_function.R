library(prophet)

options(scipen=999)


#------------------------------------------------------------------------------

f_estima_modelo <- function(dff, 
                            p.changepoint.range,
                            p.changepoint.prior.scale,
                            p.seasonality.mode,
                            p.yearly.seasonality,
                            p.semiyearly.seasonality,
                            p.monthly.seasonality,
                            p.biweekly.seasonality,
                            p.weekly.seasonality,
                            p.holidays.prior.scale,
                            p.holidays,
                            p.regressors = list()){

  d_estimation <- data.frame("p.changepoint.range" = p.changepoint.range,
                             "p.changepoint.prior.scale" = p.changepoint.prior.scale,
                             "p.seasonality.mode" = p.seasonality.mode,
                             "p.yearly.seasonality" = p.yearly.seasonality,
                             "p.semiyearly.seasonality" = p.semiyearly.seasonality,
                             "p.monthly.seasonality" = p.monthly.seasonality,
                             "p.biweekly.seasonality" = p.biweekly.seasonality,
                             "p.weekly.seasonality" = p.weekly.seasonality,
                             "p.holidays.prior.scale" = p.holidays.prior.scale)
  
  p.yearly.seasonality <- ifelse(p.yearly.seasonality == 0, FALSE, p.yearly.seasonality)
  p.semiyearly.seasonality <- ifelse(p.semiyearly.seasonality == 0, FALSE, p.semiyearly.seasonality)
  p.monthly.seasonality <- ifelse(p.monthly.seasonality == 0, FALSE, p.monthly.seasonality)
  p.biweekly.seasonality <- ifelse(p.biweekly.seasonality == 0, FALSE, p.biweekly.seasonality)
  p.weekly.seasonality <- ifelse(p.weekly.seasonality == 0, FALSE, p.weekly.seasonality)
  
  # some paramters
  set.seed(28)
  
  # parameters for the model
  m <- prophet(# trend
               changepoint.range = p.changepoint.range,
               n.changepoints = 50,
               changepoint.prior.scale = p.changepoint.prior.scale,
               # seasonality
               seasonality.mode = p.seasonality.mode,
               yearly.seasonality = p.yearly.seasonality,
               weekly.seasonality = p.weekly.seasonality,
               # holidays
               holidays = p.holidays,
               holidays.prior.scale = p.holidays.prior.scale)
  
  # additional seasonalities
  if (p.semiyearly.seasonality != FALSE){
    m <- add_seasonality(m, name = 'semiyearly', period = 182.5, fourier.order = p.semiyearly.seasonality)
  }
  
  if (p.monthly.seasonality != FALSE){
    m <- add_seasonality(m, name = 'monthly', period = 30.5, fourier.order = p.monthly.seasonality)
  }
  
  if (p.biweekly.seasonality != FALSE){
    m <- add_seasonality(m, name = 'biweekly', period = 15.25, fourier.order = p.biweekly.seasonality)
  }
  
  # additional regressors
  if (length(p.regressors) > 0){
    
    for (k in 1:length(p.regressors)){
      
      dff[[names(p.regressors)[k]]] <- p.regressors[[k]]
      m <- add_regressor(m, names(p.regressors)[k])
      
    }
    
  }
  
  # fit model
  m <- fit.prophet(m, dff)
  
  # cross validation - estim until 2019 H1 - forecast 2019 H2
  df.cv <- cross_validation(m, initial = which(df$ds == "2019-06-29"), period = 182, horizon = 365, units = 'days')
  df.p <- performance_metrics(df.cv, rolling_window = 30/365)
  
  d_estimation$mape_19h2_30 <- df.p$mape[1]
  d_estimation$coverage_19h2_30 <- df.p$coverage[1]
  
  # cross validation - estim until 2019 H2 - forecast 2020 H1
  df.cv <- cross_validation(m, initial = which(df$ds == "2019-12-30"), period = 182, horizon = 182, units = 'days')
  df.p <- performance_metrics(df.cv, rolling_window = 30/182)
  
  d_estimation$mape_20h1_30 <- df.p$mape[1]
  d_estimation$coverage_20h1_30 <- df.p$coverage[1]
  
  return(d_estimation)

}






#------------------------------------------------------------------------------

f_estima_pronosticos <- function(dff, 
                                 p.changepoint.range,
                                 p.changepoint.prior.scale,
                                 p.seasonality.mode,
                                 p.yearly.seasonality,
                                 p.semiyearly.seasonality,
                                 p.monthly.seasonality,
                                 p.biweekly.seasonality,
                                 p.weekly.seasonality,
                                 p.holidays.prior.scale,
                                 p.holidays,
                                 p.regressors = list(),
                                 p.periods,
                                 p.regressors.forecast = list()){
  
  p.yearly.seasonality <- ifelse(p.yearly.seasonality == 0, FALSE, p.yearly.seasonality)
  p.semiyearly.seasonality <- ifelse(p.semiyearly.seasonality == 0, FALSE, p.semiyearly.seasonality)
  p.monthly.seasonality <- ifelse(p.monthly.seasonality == 0, FALSE, p.monthly.seasonality)
  p.biweekly.seasonality <- ifelse(p.biweekly.seasonality == 0, FALSE, p.biweekly.seasonality)
  p.weekly.seasonality <- ifelse(p.weekly.seasonality == 0, FALSE, p.weekly.seasonality)
  
  # some paramters
  set.seed(28)
  
  # parameters for the model
  m <- prophet(# trend
    changepoint.range = p.changepoint.range,
    n.changepoints = 50,
    changepoint.prior.scale = p.changepoint.prior.scale,
    # seasonality
    seasonality.mode = p.seasonality.mode,
    yearly.seasonality = p.yearly.seasonality,
    weekly.seasonality = p.weekly.seasonality,
    # holidays
    holidays = p.holidays,
    holidays.prior.scale = p.holidays.prior.scale)
  
  # additional seasonalities
  if (p.semiyearly.seasonality != FALSE){
    m <- add_seasonality(m, name = 'semiyearly', period = 182.5, fourier.order = p.semiyearly.seasonality)
  }
  
  if (p.monthly.seasonality != FALSE){
    m <- add_seasonality(m, name = 'monthly', period = 30.5, fourier.order = p.monthly.seasonality)
  }
  
  if (p.biweekly.seasonality != FALSE){
    m <- add_seasonality(m, name = 'biweekly', period = 15.25, fourier.order = p.biweekly.seasonality)
  }
  
  # additional regressors
  if (length(p.regressors) > 0){
    
    for (k in 1:length(p.regressors)){
      
      dff[[names(p.regressors)[k]]] <- p.regressors[[k]]
      m <- add_regressor(m, names(p.regressors)[k])
      
    }
    
  }
  
  # fit model
  m <- fit.prophet(m, dff)
  
  # predict 
  future <- make_future_dataframe(m, periods = p.periods)
  
  if (length(p.regressors.forecast) > 0){
    
    for (k in 1:length(p.regressors.forecast)){
      
      future %<>% left_join(dff[c("ds", names(p.regressors.forecast)[k])], by = "ds")
      reg_aux <- p.regressors.forecast[[k]]
      names(reg_aux)[names(reg_aux) == names(p.regressors.forecast)[k]] <- "pronos"
      future %<>% left_join(reg_aux, by = "ds")
      future[[names(p.regressors.forecast)[k]]] <- ifelse(is.na(future[[names(p.regressors.forecast)[k]]]),
                                                          yes = future[["pronos"]],
                                                          no = future[[names(p.regressors.forecast)[k]]])
      future <- future[, names(future)[names(future) != "pronos"]]
      
    }
    
  }
  
  forecast <- predict(m, future)
  
  return(list("m" = m, "forecast" = forecast))
  
}



