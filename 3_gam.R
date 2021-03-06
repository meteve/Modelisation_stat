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
  gam_date <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(month,k=12,bs='cc') +
                    s(prev_day_price,k=50,bs="tp") + s(hour, k=24, bs='cc') +
                    s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp")+
                    s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") +
                    s(Forecasted_Zonal_Load, bs="tp") + s(cubzonalload,k=100,bs="tp") +
                    s(Forecasted_Total_Load, bs="tp") + s(sqrtotalload,k=50,bs="tp") +
                    s(cubtotalload,k=100,bs="tp"),
                  data = prices[169:(index-1),])
  pred <- predict(gam_date, newdata = filter(df_pred, grepl(pattern = date, df_pred$timestamp)))
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('r2_adj' = summary(gam_date)$r.sq, 'rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


#gam_130606 <- get_gam(date = '2013-06-06')


#appliquons cette fonction a toutes les dates que nous devons predire

date_pred <- c('2013-06-06', '2013-06-17', '2013-06-24', '2013-07-04',
               '2013-07-09', '2013-07-13', '2013-07-16', '2013-07-18',
               '2013-07-19', '2013-07-20', '2013-07-24', '2013-07-25',
               '2013-12-06', '2013-12-07', '2013-12-17')

gam_pred <- paste0('gam_', date_pred)
gam_pred <- gsub("-", "", gam_pred)


R2_adj <- NULL
RMSE <- NULL
MAE <- NULL
pred <- NULL


#on sauvegarde les resultats (R2 adj, pred, rmse et mae)

for (i in (1:length(date_pred))){
  res_gam <- get_gam(date_pred[i])
  R2_adj <- c(R2_adj, res_gam[1])
  RMSE <- c(RMSE, res_gam[2])
  MAE <- c(MAE, res_gam[3])
  pred <- c(pred, res_gam[4:27])
  
}


#sauvegarder les resultats dans deux tables

#TABLE 1 : table des erreurs et force du modele
#variables : R2_adj, rmse, mae
#individus : dates a predire

df_gam_err <- data.frame(date_pred, R2_adj, RMSE, MAE)


#TABLE 2 : table des predictions
#variables : dates
#individus : heures 
#valeurs : pred

heures <- 0:24
df_gam_pred <- data.frame(heures, matrix(pred, nrow = 24))
colnames(df_gam_pred) <- c('hour', date_pred)

#on sauvegarde les resultats au format csv
write_csv(df_gam_err, "data/df_gam_err.csv")
write_csv(df_gam_pred, "data/df_gam_pred.csv")


