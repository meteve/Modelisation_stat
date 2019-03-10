
library(tidyverse)

prices <- read_csv(file = "data/tidy_prices.csv")




# ANALYSE BIVARIEE --------------------------------------------------------



#### variables continues de demande locale et totale

ggplot(prices) +
  geom_point(mapping = aes(x = Forecasted_Total_Load, y = Forecasted_Zonal_Load))

# relation lineaire entre les variables de demande




#### variables de demande en fonction du prix

ggplot(prices) +
  geom_point(mapping = aes(x = Zonal_Price, y = Forecasted_Total_Load))

ggplot(prices) +
  geom_point(mapping = aes(x = Zonal_Price, y = Forecasted_Zonal_Load))




#### variables de prix et de date

# sur la serie complete (toutes les dates)

ggplot(prices) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))



# par annee
prices2011 <- filter(prices, grepl(pattern = '2011.', timestamp))
prices2012 <- filter(prices, grepl(pattern = '2012.', timestamp))
prices2013 <- filter(prices, grepl(pattern = '2013.', timestamp))


ggplot(prices2011) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))
ggplot(prices2012) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))
ggplot(prices2013) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))



#### Suite analyse bivariée : 
#############################


ggplot(prices)+
  geom_point(mapping = aes(x = Zonal_Price, y = prev_day_price))

ggplot(prices)+
  geom_point(mapping = aes(x = Zonal_Price, y = prev_week_price))

ggplot(prices)+
  geom_point(mapping = aes(x = prev_week_price, y = prev_day_price))



# test modif

