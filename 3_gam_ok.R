library(tidyverse)
library(mgcv)
library(stargazer)

prices <- read_csv(file = "data/tidy_prices.csv")
df_pred <- read_csv(file = "data/df_pred.csv")




# fonctions qui retournent la rmse et la mae pour y et yhat donnes
get_rmse <- function(y, yhat){
  rmse <- sqrt(mean((y-yhat)^2))
  return(rmse)
}
get_mae <- function(y,yhat){
  mae <- mean(abs(y-yhat))
  return(mae)
}

# GAM ---------------------------------------------------------------------

# Topo sur la syntaxe : 
# La forme générale peut être constatée dans le modèle ci dessous
# s est une fonction, on peut paramétrer bs : le type de spline, k le nombre de noeuds
# plot(gam) donne des plots pour les effets NON LINEAIRES


# fonction qui estime le modele gam pour une date donnee sur toutes les dates ulterieures
get_gam <- function(date){
  index <- which(grepl(pattern = date, prices$timestamp))[1]
  gam_date <- gam(formula = Zonal_Price ~ s(daynum, k = 7, bs = 'cc') +
                    s(month, k = 12, bs = 'cc') + s(hour, k = 24, bs = 'cc') +
                    s(Forecasted_Zonal_Load, k = 35, bs = 'tp') +
                    s(Forecasted_Total_Load, k = 32, bs = 'tp') +
                    s(prev_day_price, k = 25, bs = 'tp') +  s(prev_week_price, k = 27, bs = 'tp') +
                    s(Max_price, k = 50, bs = 'tp') + s(Min_price, k = 50, bs = 'tp'), 
                  family = Gamma(link = log),
                  data = prices[169:(index-1),])
  pred <- predict(gam_date, newdata = filter(df_pred, grepl(pattern = date, df_pred$timestamp)))
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('r2_adj' = summary(gam_date)$r.sq, 'rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


#appliquons cette fonction a toutes les dates que nous devons predire

date_pred <- c('2013-06-06', '2013-06-17', '2013-06-24', '2013-07-04',
               '2013-07-09', '2013-07-13', '2013-07-16', '2013-07-18',
               '2013-07-19', '2013-07-20', '2013-07-24', '2013-07-25',
               '2013-12-06', '2013-12-07', '2013-12-17')

gam_pred <- paste0('gam_', date_pred)
gam_pred <- gsub("-", "", gam_pred)




#on sauvegarde les resultats (R2 adj, pred, rmse et mae)
R2_adj <- vector("numeric", 15)
RMSE <- vector("numeric", 15)
MAE <- vector("numeric", 15)
pred <- vector("list", 15)
for (i in (1:length(date_pred))){
  res_gam <- get_gam(date_pred[i])
  R2_adj[i] <- res_gam[1]
  RMSE[i] <- res_gam[2]
  MAE[i] <- res_gam[3]
  pred[[i]] <- res_gam[4:27]
}
pred <- unlist(pred)

#sauvegarder les resultats dans deux tables
#TABLE 1 : table des erreurs et force du modele
#variables : R2_adj, rmse, mae
#individus : dates a predire
df_gam_err <- data.frame(date_pred, R2_adj, RMSE, MAE)

#TABLE 2 : table des predictions
#variables : dates
#individus : heures 
#valeurs : pred
heures <- 0:23
df_gam_pred <- data.frame(heures, matrix(pred, nrow = 24))
colnames(df_gam_pred) <- c('hour', date_pred)

#on sauvegarde les resultats au format csv
write_csv(df_gam_err, "data/df_gam_err.csv")
write_csv(df_gam_pred, "data/df_gam_pred.csv")

df_gam_err <- read_csv(file = "data/df_gam_err.csv",
                       col_types = cols(col_character(), col_double(),
                                        col_double(), col_double()))
df_gam_pred <- read_csv(file = "data/df_gam_pred.csv")

#on sort les resultats
stargazer(df_gam_err, summary = FALSE)
stargazer(round(df_gam_pred, 2), summary = FALSE)



#pourquoi de si grandes erreurs en juillet 2013 et en décembre 2013?
plot_price_month <- function(date){
  index <- which(prices$timestamp == date)
  prices_month <- prices[index:(index+720),]
  p <- ggplot(prices_month) +
    geom_line(mapping = aes(x = timestamp, y = Zonal_Price)) +
    xlab("") + ylab("Prix dans la zone étudiée")
  return(p)
}

plot_price_month('2013-07-01') #tres forte hausse des prix du 16 au 20 juillet
plot_price_month('2013-12-01') #forte hausse le 17 decembre