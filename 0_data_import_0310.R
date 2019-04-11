library(tidyverse)
library(mgcv)
library(lubridate)
library(fastDummies)
library(plyr)

#rm(list=ls())

#

# IMPORTATION DES DONNEES -------------------------------------------------

# importer en convertissant directement en format 'datetime' la deuxieme colonne
prices <- read_delim("data/prices.csv", delim = ";",
                     col_types = cols('timestamp' = 
                                        col_datetime("%m%d%Y %H:%M")))



# on remplace les espaces par des underscore dans les noms de colonnes
prices <- dplyr::rename(prices, ZONEID = ZONEID, timestamp = timestamp,
                 Forecasted_Total_Load = `Forecasted Total Load`,
                 Forecasted_Zonal_Load = `Forecasted Zonal Load`,
                 Zonal_Price = `Zonal Price`)

# CREATION DE VARIABLES ---------------------------------------------------

############## DUMMIES

# dummies pour l'annee
prices$year <- year(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'year')

# dummies pour le mois
prices$month <- month(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'month')

# dummies pour le jour
prices <- prices %>%
  mutate(day = weekdays(as.Date(timestamp)))

prices$day <- tolower(prices$day)

prices <- dummy_cols(prices, select_columns = 'day')

# dummies pour le week-end 
prices$samedi<-as.numeric(prices$day_samedi==1)
prices$dimanche<-as.numeric(prices$day_dimanche==1)
prices$weekend<-(prices$samedi + prices$dimanche)

# dummies pour l'heure
prices$hour <- hour(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'hour')

####### AUTRES

# prix a la journee precedente 
previous_day_price <- apply(as.matrix(25:nrow(prices)), 1,
                            function(i){prices$Zonal_Price[(i-24)]})
previous_day_price_NA <- rep(NA, 24)

prices$prev_day_price <- c(previous_day_price_NA, previous_day_price)

# prix à deux jours avant 
previous_day2_price <- apply(as.matrix(49:nrow(prices)), 1,
                             function(i){prices$Zonal_Price[(i-48)]})
previous_day2_price_NA <- rep(NA, 48)

prices$prev_day2_price <- c(previous_day2_price_NA, previous_day2_price)

# prix a la semaine precedente
previous_week_price <- apply(as.matrix(169:nrow(prices)), 1,
                             function(i){prices$Zonal_Price[(i-168)]})
previous_week_price_NA <- rep(NA, 168)

prices$prev_week_price <- c(previous_week_price_NA, previous_week_price)

# prix minimum des 24h précédentes
Min_price <- apply(as.matrix(25:nrow(prices)), 1,
                   function(i){min(prices$Zonal_Price[(i-24):i])})
Min_price_NA <- rep(NA, 24)

prices$Min_price <- c(Min_price_NA, Min_price)

# prix maximum des 24h précédentes
Max_price <- apply(as.matrix(25:nrow(prices)), 1,
                   function(i){max(prices$Zonal_Price[(i-24):i])})
Max_price_NA<-rep(NA,24)

prices$Max_price<-c(Max_price_NA, Max_price)

# load au carré (Total Load)
prices$sqrtotalload <- (prices$Forecasted_Total_Load)^2

# load au carré (Zonal Load)
prices$sqrzonalload <- (prices$Forecasted_Zonal_Load)^2

# load au cube (Total Load)
prices$cubtotalload <- (prices$Forecasted_Total_Load)^3

# load au cube (Zonal Load)
prices$cubzonalload <- (prices$Forecasted_Zonal_Load)^3

# Association d'une valeur numérique à chaque jour de la semaine
prices$daynum <- as.integer(factor(prices$day,
                                   levels = c("lundi", "mardi", "mercredi",
                                              "jeudi","vendredi","samedi","dimanche")))

#### Création de bases de données par année : ####
prices2011 <- filter(prices, grepl(pattern = '2011.', timestamp))
prices2012 <- filter(prices, grepl(pattern = '2012.', timestamp))
prices2013 <- filter(prices, grepl(pattern = '2013.', timestamp))


# TABLES A PREDIRE --------------------------------------------------------

#fonction qui retourne pour une date donnee la table des variables a chaque heure
get_df_pred <- function(x){
  df_pred <- filter(prices, grepl(pattern = x, timestamp))
  return(df_pred)
}

#date a predire
date_pred <- c('2013-06-06', '2013-06-17', '2013-06-24', '2013-07-04',
               '2013-07-09', '2013-07-13', '2013-07-16', '2013-07-18',
               '2013-07-19', '2013-07-20', '2013-07-24', '2013-07-25',
               '2013-12-06', '2013-12-07', '2013-12-17')

#liste des tables de donnees pour chaque date a predire
list_df_pred <- lapply(date_pred, get_df_pred)
#dataframe des dates a predire
df_pred <- ldply(list_df_pred)
head(df_pred)

# EXPORTER DONNEES --------------------------------------------------------

write_csv(df_pred, "data/df_pred.csv")
write_csv(prices, "data/tidy_prices.csv")



