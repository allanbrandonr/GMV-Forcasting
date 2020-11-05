library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)

options(scipen=999)

datos <- read_csv("Data_Scientist_Assessment_Data_ok.csv")

dir.create("02_glimpse")

summary(datos)

datos$year <- cut(datos$date,"year") %>% as.Date() 
datos$quarter <- cut(datos$date,"quarter") %>% as.Date() 
datos$month <- cut(datos$date,"month") %>% as.Date() 
datos$week <- cut(datos$date,"week") %>% as.Date() 

nom_vars <- names(datos)[!names(datos) %in% c("date", "year", "quarter", "month", "week")]



# correlations with gmv
correls <- cor(datos[nom_vars], use = "complete.obs") 
correls <- data.frame("var1" = row.names(correls), correls, row.names = NULL)
correls %<>% gather(key = "var2", value = "correl", -var1) %>% 
  filter(var1 == "gmv" & var1 != var2) %>%
  arrange(desc(abs(correl)))

write.csv(correls, "02_glimpse/correls.csv", row.names = F)

# correlations with ads spend
correls_2 <- cor(datos[nom_vars], use = "complete.obs") 
correls_2 <- data.frame("var1" = row.names(correls_2), correls_2, row.names = NULL)
correls_2 %<>% gather(key = "var2", value = "correl", -var1) %>% 
  filter(var1 == "ads_spend" & var1 != var2) %>%
  arrange(desc(abs(correl)))

correls_2 <- left_join(correls %>% select(var2), correls_2, by = "var2") %>%
  select(var1, var2, correl) %>%
  filter(!is.na(var1))

write.csv(correls_2, "02_glimpse/correls_2.csv", row.names = F)

# create some graphs

for (nom_var in nom_vars){

  # nom_var <- nom_vars[1]
  
  dir.create(paste0("02_glimpse/",nom_var))
  
  # boxplots por trimestre
  p <- ggplot(datos) +
    geom_boxplot(aes(x = quarter, y = get(nom_var), group = quarter)) +
    theme_minimal() +
    labs(x = "quarter", y = nom_var, title = nom_var) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  png(paste0("02_glimpse/",nom_var,"/",nom_var, "_boxplot.png"), width = 800, height = 500)
  print(p)
  dev.off()
  
  if(nom_var == "gmv"){
    
    # serie promedio semanal
    datos_aux_1 <-datos %>% group_by(week) %>% summarise(average = sum(get(nom_var), na.rm = T))
    
    p <- ggplot(datos_aux_1) +
      geom_line(aes(x = week, y = average, group = 1)) +
      theme_minimal() +
      labs(x = "week", y = nom_var, title = nom_var) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    png(paste0("02_glimpse/",nom_var,"/",nom_var, "_series.png"), width = 800, height = 500)
    print(p)
    dev.off()
    
  } else {
  
    # series promedio semanal
    datos_aux_2 <-datos %>% group_by(week) %>% 
      summarise(average_var = sum(get(nom_var), na.rm = T),
                average_gmv = sum(gmv, na.rm = T))
    
    cor_2 <- cor(datos_aux_2$average_var, datos_aux_2$average_gmv) %>% round(2)
    
    rango_ym <- max(datos_aux_2$average_gmv) - min(datos_aux_2$average_gmv)
    min_ym <- max(min(datos_aux_2$average_gmv), 0)
    
    rango_var <- max(datos_aux_2$average_var) - min(datos_aux_2$average_var)
    
    p <- ggplot(datos_aux_2) +
      geom_line(aes(x = week, y = average_var, group = 1)) +
      geom_line(aes(x = week, y = (average_gmv - min_ym)/rango_ym*rango_var, group = 1), linetype="dashed", color = "darkblue") +
      theme_minimal() +
      scale_y_continuous(sec.axis = sec_axis(~.*rango_ym/rango_var + min_ym, name = "gmv")) +
      labs(x = "week", y = nom_var, title = paste0(nom_var, " vs gmv"),
           subtitle = paste0("cor: ", cor_2)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    png(paste0("02_glimpse/",nom_var,"/",nom_var, "_series.png"), width = 800, height = 500)
    print(p)
    dev.off()
    
    # grafico de dispersion
    cor_3 <- cor(datos[[nom_var]], datos$gmv, "pairwise.complete.obs") %>% round(2)
    
    p <- ggplot(datos) +
      geom_point(aes(x = get(nom_var), y = gmv, color = factor(year)), alpha = .4) +
      geom_smooth(aes(x = get(nom_var), y = gmv), method = "lm", se = F, linetype = "dashed", size = .5, color = "darkblue") +
      theme_minimal() +
      labs(x = nom_var, y = "gmv", title = paste0(nom_var, " vs gmv"), color = "year",
           subtitle = paste0("cor: ", cor_3))
    png(paste0("02_glimpse/",nom_var,"/",nom_var, "_dispersion.png"), width = 800, height = 500)
    print(p)
    dev.off()
  
  }
  
}






