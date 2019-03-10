
library(tidyverse)

prices <- read_csv(file = "data/tidy_prices.csv")



# BENCHMARK ---------------------------------------------------------------

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

