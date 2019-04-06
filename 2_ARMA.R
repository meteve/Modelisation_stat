library(tidyverse)
library(zoo)

prices <- read_csv(file = "data/tidy_prices.csv")


#conversion en un objet serie temp
prices.ts <- zoo(prices$Zonal_Price)
class(prices.ts)
plot(prices.ts)
plot(prices.ts[1:168,]) #saisonnalite journaliere, ordre 24

desaison <- prices.ts - lag(prices.ts, k = -24)
plot(desaison[1:168,])


acf(desaison, na.action = na.pass, lag.max = 50000)
pacf(desaison, na.action = na.pass, lag.max = 1000)



