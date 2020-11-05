library(dplyr)
library(magrittr)
library(readxl)
library(anytime)

# read data and correctly parse the date
datos_a <- read_xlsx("Data_Scientist_Assessment_Data.xlsx")
datos_b <- read_xlsx("Data_Scientist_Assessment_Data.xlsx",col_types = c("date", rep("numeric",34)))

datos_a$date[!is.na(as.numeric(datos_a$date))] <- NA
datos_a$date %<>% as.Date(format = "%d-%m-%y")

datos_b$date %<>% as.character()
datos_b$date %<>% as.Date(format = "%Y-%d-%m")

datos <- datos_a
datos$date <- ifelse(is.na(datos_a$date), yes = datos_b$date, no = datos_a$date) %>% anydate()

rm(datos_a, datos_b)

# save data
write.csv(datos, "Data_Scientist_Assessment_Data_ok.csv", row.names = F)