
library(tidyverse)
library(zoo)

prices <- read_csv(file = "data/tidy_prices.csv")



# STATIONNARISATION DE LA SERIE -------------------------------------------

# Transformation de la série au format ts (time series)
year_start <- prices$year[1]
hour_start <- prices$hour[1]
year_end <- prices$year[nrow(prices)]
hour_end <- prices$hour[nrow(prices)]

prices_ts <- ts(prices$Zonal_Price, frequency = 365*24) 



# desaisonnaliser

decomposition <- decompose(prices_ts) # on decompose la serie temp
plot(decomposition)
prices_desaiso = prices_ts / decomposition$seasonal

plot(prices_desaiso) # a quoi ca correspond??

acf(prices_desaiso) # autocorrelations??




prices$daynum=as.integer(factor(prices$day, levels = c("lundi", "mardi", "mercredi","jeudi","vendredi","samedi","dimanche"))
prices$daynum=daynum
                         




#jerem

zebi=ar(prices$Zonal_Price, aic = TRUE, order.max = NULL)
summary(zebi)

zebi1= ar(prices$Zonal_Price,method="burg", aic = TRUE, order.max = NULL)
summary(zebi1)

zebi2=ar(prices$Zonal_Price, method="ols", aic = TRUE, order.max = NULL)
summary(zebi2)

zebi3=ar(prices$Zonal_Price, method="mle", aic = TRUE, order.max = NULL)
summary(zebi3)

zebi4=ar(prices$Zonal_Price, method="yw", aic = TRUE, order.max = NULL)
summary(zebi4)




class(prices$day)




prices$daynum=as.integer(factor(prices$day, levels = c("lundi", "mardi", "mercredi","jeudi","vendredi","samedi","dimanche")))              

