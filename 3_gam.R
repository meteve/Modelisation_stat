
library(tidyverse)
library(mgcv)


prices <- read_csv(file = "data/tidy_prices.csv")



# GAM ---------------------------------------------------------------------

gam_1 <- gam(formula = Zonal_Price ~ s(day) + prev_day_price + prev_week_price +
               Min_Price, data = prices[169:nrow(prices),])


summary(gam_1)
