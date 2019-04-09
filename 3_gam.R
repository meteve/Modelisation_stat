
library(tidyverse)
library(mgcv)
library(stargazer)

prices <- read_csv(file = "data/tidy_prices.csv")
df_pred <- read_csv(file = "data/df_pred.csv")



# GAM ---------------------------------------------------------------------

# Topo sur la syntaxe : 
# La forme générale peut être constatée dans le modèle ci dessous
# s est une fonction, on peut paramétrer bs : le type de spline, k le nombre de noeuds
# plot(gam) donne des plots pour les effets NON LINEAIRES


# gam_1 <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(prev_day_price,k=50,bs="tp") +
#                s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp") +
#                s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") +
#                s(Forecasted_Zonal_Load) + s(cubzonalload,k=100,bs="tp"), 
#              data = prices[169:nrow(prices),])
# 
# names(gam_1)
# summary(gam_1)
# plot(gam_1)





#on ajoute des variables
# gam_ok <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(month,k=12,bs='cc') +
#                s(prev_day_price,k=50,bs="tp") + s(hour, k=24, bs='cc') +
#                s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp")+
#                s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") +
#                s(Forecasted_Zonal_Load,bs="tp") + s(cubzonalload,k=100,bs="tp") +
#                s(Forecasted_Total_Load) + s(sqrtotalload,k=50,bs="tp") +
#                s(cubtotalload,k=100,bs="tp"),
#              data = prices[169:nrow(prices),])

# 
# summary(gam_2)
# gam.check(gam_2)

# en changeant les degres de liberte
# gam_3 <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(month,k=12,bs='cc') +
#                s(prev_day_price,k=50,bs="tp") + s(hour, k=24, bs='cc') +
#                s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp")+
#                s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") + 
#                s(Forecasted_Zonal_Load, bs="tp") + s(cubzonalload,k=100,bs="tp") +
#                s(Forecasted_Total_Load, bs="tp") + s(sqrtotalload,k=50,bs="tp") +
#                s(cubtotalload,k=100,bs="tp"), 
#              data = prices[169:nrow(prices),])
# 
# summary(gam_3)
# 
# gam.check(gam_3)


#creer le vecteur des prix a predire
# total_pred <- predict(gam_2, newdata = df_pred)



# fonctions qui retournent la rmse et la mae pour y et yhat donnes
get_rmse <- function(y, yhat){
  rmse <- sqrt(mean((y-yhat)^2))
  return(rmse)
}

get_mae <- function(y,yhat){
  mae <- mean(abs(y-yhat))
  return(mae)
}



# fonction qui estime le modele gam pour une date donnee sur toutes les dates ulterieures

get_gam <- function(date){
  index <- which(grepl(pattern = date, prices$timestamp))[1]
  gam <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(month,k=12,bs='cc') +
                       s(prev_day_price,k=50,bs="tp") + s(hour, k=24, bs='cc') +
                       s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp")+
                       s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") +
                       s(Forecasted_Zonal_Load, bs="tp") + s(cubzonalload,k=100,bs="tp") +
                       s(Forecasted_Total_Load, bs="tp") + s(sqrtotalload,k=50,bs="tp") +
                       s(cubtotalload,k=100,bs="tp"),
                     data = prices[169:(index-1),])
  pred <- predict(gam, newdata = filter(df_pred, grepl(pattern = date, prices$timestamp)))
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  #return('gam' = gam, 'pred' = pred, 'rmse' = rmse, 'mae' = mae)
}


get_gam(date = '2013-06-06')




