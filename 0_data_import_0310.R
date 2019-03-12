
library(tidyverse)
library(mgcv)
library(lubridate)
library(fastDummies)

#rm(list=ls())



# IMPORTATION DES DONNEES -------------------------------------------------


# importer en convertissant directement en format 'datetime' la deuxieme colonne
prices <- read_delim("data/prices.csv", delim = ";",
                   col_types = cols('timestamp' = 
                                      col_datetime("%m%d%Y %H:%M")))



# on remplace les espaces par des underscore dans les noms de colonnes
prices <- rename(prices, ZONEID = ZONEID, timestamp = timestamp,
               Forecasted_Total_Load = `Forecasted Total Load`,
               Forecasted_Zonal_Load = `Forecasted Zonal Load`,
               Zonal_Price = `Zonal Price`)







# CREATION DE VARIABLES ---------------------------------------------------

#### jour de la semaine

prices <- prices %>%
  mutate(day = weekdays(as.Date(timestamp)))

prices$day <- tolower(prices$day)



#### prix a la journee precedente

previous_day_price <- apply(as.matrix(25:nrow(prices)), 1,
                            function(i){prices$Zonal_Price[(i-24)]})
previous_day_price_NA <- rep(NA, 24)

prices$prev_day_price <- c(previous_day_price_NA, previous_day_price)


### Prix à deux jours avant : 

previous_day2_price <- apply(as.matrix(49:nrow(prices)), 1,
                            function(i){prices$Zonal_Price[(i-48)]})
previous_day2_price_NA <- rep(NA, 48)

prices$prev_day2_price <- c(previous_day2_price_NA, previous_day2_price)



#### prix a la semaine precedente

previous_week_price <- apply(as.matrix(169:nrow(prices)), 1,
                            function(i){prices$Zonal_Price[(i-168)]})
previous_week_price_NA <- rep(NA, 168)

prices$prev_week_price <- c(previous_week_price_NA, previous_week_price)


#### prix minimum des 24h precedentes

Min_Price <- apply(as.matrix(25:nrow(prices)), 1,
                   function(i){min(prices$Zonal_Price[(i-24):i])})
Min_Price_NA <- rep(NA, 24)

prices$Min_Price <- c(Min_Price_NA, Min_Price)


#### dummies jour de la semaine

prices <- dummy_cols(prices, select_columns = 'day')


#### dummies heure de la journee

prices$hour <- hour(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'hour')




#### load au carré (Total Load)

prices$sqrtotalload <- (prices$Forecasted_Total_Load)^2

#### load au carré (Zonal Load)

prices$sqrzonalload <- (prices$Forecasted_Zonal_Load)^2

#### load au cube (Total Load)

prices$cubtotalload <- (prices$Forecasted_Total_Load)^3

#### load au cube (Zonal Load)

prices$cubzonalload <- (prices$Forecasted_Zonal_Load)^3





# EXPORTER DONNEES --------------------------------------------------------

write_csv(prices, "data/tidy_prices.csv")





