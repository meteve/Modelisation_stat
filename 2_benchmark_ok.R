library(tidyverse)
library(zoo)
library(stargazer)
library(broom)
library(MASS)
library(tree)
library(glmnet)

prices <- read_csv(file = "data/tidy_prices.csv")
df_pred <- read_csv(file = "data/df_pred.csv")

#fonctions de calcul des erreurs
get_rmse <- function(y, yhat){
  rmse <- sqrt(mean((y-yhat)^2))
  return(rmse)
}
get_mae <- function(y,yhat){
  mae <- mean(abs(y-yhat))
  return(mae)
}

#dates a predire
date_pred <- c('2013-06-06', '2013-06-17', '2013-06-24', '2013-07-04',
               '2013-07-09', '2013-07-13', '2013-07-16', '2013-07-18',
               '2013-07-19', '2013-07-20', '2013-07-24', '2013-07-25',
               '2013-12-06', '2013-12-07', '2013-12-17')



# Ridge -------------------------------------------------------------------

#fonction qui fit un ridge pour une date en question, predit et retourne les predictions et erreurs
get_ridge <- function(date){
  index <- which(grepl(pattern = date, prices$timestamp))[1]
  X <- as.matrix(prices[169:(index-1), c('daynum','prev_day_price','prev_day2_price',
                                         'prev_week_price','Min_Price','Max_price',
                                         'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  Y <- as.matrix(prices$Zonal_Price[169:(index-1)])
  ridge.mod <- glmnet(X, Y, alpha = 0)
  cv.out <- cv.glmnet(X, Y, alpha = 0)
  bestlam <- cv.out$lambda.min
  newX <- as.matrix(prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
                                               'prev_week_price','Min_Price','Max_price',
                                               'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  pred <- predict(ridge.mod, s = bestlam, newx = newX)
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


#boucle sur les dates a predire
RMSE <- NULL
MAE <- NULL
pred <- NULL
for (i in (1:length(date_pred))){
  res_ridge <- get_ridge(date_pred[i])
  RMSE <- c(RMSE, res_ridge[1])
  MAE <- c(MAE, res_ridge[2])
  pred <- c(pred, res_ridge[3:26])
}

#sauvegarder les resultats dans deux tables
#TABLE 1 : table des erreurs et force du modele
#variables : R2_adj, rmse, mae
#individus : dates a predire
df_ridge_err <- data.frame(date_pred, RMSE, MAE)

#TABLE 2 : table des predictions
#variables : dates
#individus : heures 
#valeurs : pred
heures <- 0:23
df_ridge_pred <- data.frame(heures, matrix(pred, nrow = 24))
colnames(df_ridge_pred) <- c('hour', date_pred)

#on sauvegarde les resultats au format csv
write_csv(df_ridge_err, "data/df_ridge_err.csv")
write_csv(df_ridge_pred, "data/df_ridge_pred.csv")

#on sort les resultats
stargazer(df_ridge_err, summary = FALSE)
stargazer(round(df_ridge_pred, 2), summary = FALSE)



# LASSO -------------------------------------------------------------------

get_lasso <- function(date){
  index <- which(grepl(pattern = date, prices$timestamp))[1]
  X <- as.matrix(prices[169:(index-1), c('daynum','prev_day_price','prev_day2_price',
                                         'prev_week_price','Min_Price','Max_price',
                                         'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  Y <- as.matrix(prices$Zonal_Price[169:(index-1)])
  lasso.mod <- glmnet(X, Y, alpha = 1)
  cv.out <- cv.glmnet(X, Y, alpha = 1)
  bestlam <- cv.out$lambda.min
  newX <- as.matrix(prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
                                               'prev_week_price','Min_Price','Max_price',
                                               'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  pred <- predict(lasso.mod, s = bestlam, newx = newX)
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


#boucle sur les dates a predire
RMSE <- NULL
MAE <- NULL
pred <- NULL
for (i in (1:length(date_pred))){
  res_lasso <- get_lasso(date_pred[i])
  RMSE <- c(RMSE, res_lasso[1])
  MAE <- c(MAE, res_lasso[2])
  pred <- c(pred, res_lasso[3:26])
}

#sauvegarder les resultats dans deux tables
#TABLE 1 : table des erreurs et force du modele
#variables : R2_adj, rmse, mae
#individus : dates a predire
df_lasso_err <- data.frame(date_pred, RMSE, MAE)

#TABLE 2 : table des predictions
#variables : dates
#individus : heures 
#valeurs : pred
heures <- 0:23
df_lasso_pred <- data.frame(heures, matrix(pred, nrow = 24))
colnames(df_lasso_pred) <- c('hour', date_pred)

#on sauvegarde les resultats au format csv
write_csv(df_lasso_err, "data/df_lasso_err.csv")
write_csv(df_lasso_pred, "data/df_lasso_pred.csv")

#on sort les resultats
stargazer(df_lasso_err, summary = FALSE)
stargazer(round(df_lasso_pred, 2), summary = FALSE)
