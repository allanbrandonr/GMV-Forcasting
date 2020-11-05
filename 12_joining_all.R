library(dplyr)
library(magrittr)
library(readr)

# cargar datos
datos <- read_csv("Data_Scientist_Assessment_Data_ok.csv")

fcst_as <- read_csv("11_final_forecasts/forecast_ads_spend.csv")
fcst_ecr <- read_csv("11_final_forecasts/forecast_ecr.csv")
fcst_gmv <- read_csv("11_final_forecasts/forecast_gmv.csv") 

fcst_gmv_as <- read_csv("11_final_forecasts/forecast_gmv_vs_ads_spend.csv")
fcst_gmv_as_ecr <- read_csv("11_final_forecasts/forecast_gmv_vs_ads_spend_ecr.csv")

# cambiar nombre
fcst_as %<>% select(date = ds, ads_spend_hat = yhat)
fcst_ecr %<>% select(date = ds, effective_conversion_rate_hat = yhat)
fcst_gmv %<>% select(date = ds, gmv_hat = yhat)

fcst_gmv_as %<>% select(date = ds, gmv_as_hat = yhat)
fcst_gmv_as_ecr %<>% select(date = ds, gmv_as_ecr_hat = yhat)

# join all data
datos %<>% full_join(fcst_as, by = "date") %>%
  full_join(fcst_ecr, by = "date") %>%
  full_join(fcst_gmv, by = "date") %>%
  full_join(fcst_gmv_as, by = "date") %>%
  full_join(fcst_gmv_as_ecr, by = "date") 

datos$ads_spend_hat = ifelse(datos$date > "2020-07-21", 
                       yes = datos$ads_spend_hat, 
                       no = datos$ads_spend)

datos$effective_conversion_rate_hat = ifelse(datos$date > "2020-07-21", 
                                       yes = datos$effective_conversion_rate_hat, 
                                       no = datos$effective_conversion_rate)
datos$gmv_hat = ifelse(datos$date > "2020-07-21", 
                 yes = datos$gmv_hat, 
                 no = datos$gmv)

datos$gmv_as_hat = ifelse(datos$date > "2020-07-21", 
                    yes = datos$gmv_as_hat, 
                    no = datos$gmv)

datos$gmv_as_ecr_hat = ifelse(datos$date > "2020-07-21", 
                        yes = datos$gmv_as_ecr_hat, 
                        no = datos$gmv)
  
# guardar
write.csv(datos, "Assessment_Data_final.csv", row.names = F)
  
