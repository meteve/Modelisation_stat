library(tidyverse)
library(zoo)
library(stargazer)
library(broom)
library(MASS)
library(tree)
library(randomForest)
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
                                         'prev_week_price','Min_price','Max_price',
                                         'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  Y <- as.matrix(prices$Zonal_Price[169:(index-1)])
  ridge.mod <- glmnet(X, Y, alpha = 0)
  cv.out <- cv.glmnet(X, Y, alpha = 0)
  bestlam <- cv.out$lambda.min
  newX <- as.matrix(prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
                                               'prev_week_price','Min_price','Max_price',
                                               'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  pred <- predict(ridge.mod, s = bestlam, newx = newX)
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


#boucle sur les dates a predire
RMSE <- vector("numeric", 15)
MAE <- vector("numeric", 15)
pred <- vector("list", 15)
for (i in (1:length(date_pred))){
  res_ridge <- get_ridge(date_pred[i])
  RMSE[i] <- res_ridge[1]
  MAE[i] <- res_ridge[2]
  pred[[i]] <- res_ridge[3:26]
}
pred <- unlist(pred)

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
                                         'prev_week_price','Min_price','Max_price',
                                         'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  Y <- as.matrix(prices$Zonal_Price[169:(index-1)])
  lasso.mod <- glmnet(X, Y, alpha = 1)
  cv.out <- cv.glmnet(X, Y, alpha = 1)
  bestlam <- cv.out$lambda.min
  newX <- as.matrix(prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
                                               'prev_week_price','Min_price','Max_price',
                                               'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
  pred <- predict(lasso.mod, s = bestlam, newx = newX)
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


#boucle sur les dates a predire
RMSE <- vector("numeric", 15)
MAE <- vector("numeric", 15)
pred <- vector("list", 15)
for (i in (1:length(date_pred))){
  res_lasso <- get_lasso(date_pred[i])
  RMSE[i] <- res_lasso[1]
  MAE[i] <- res_lasso[2]
  pred[[i]] <- res_lasso[3:26]
}
pred <- unlist(pred)

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




# Arbre -------------------------------------------------------------------

# ARBRE -------------------------------------------------------------------

get_arbre <- function(date){
  index <- which(grepl(pattern = date, prices$timestamp))[1]
  X <- prices[169:(index-1), c('daynum','prev_day_price','prev_day2_price',
                               'prev_week_price','Min_price','Max_price',
                               'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
  Y <- prices$Zonal_Price[169:(index-1)]
  arbre = tree(formula = Y ~., data = X, method = 'anova')
  # cv.arbre = cv.tree(arbre)
  # prune.arbre = prune.tree(arbre,best=10)
  newX <- prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
                                     'prev_week_price','Min_price','Max_price',
                                     'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
  pred <- predict(arbre, s = 10, newx = newX)
  y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
  rmse <- get_rmse(y = y, yhat = pred)
  mae <- get_mae(y = y, yhat = pred)
  result <- c('rmse' = rmse, 'mae' = mae, 'pred' = pred)
  return(result)
}


RMSE <- vector("numeric", 15)
MAE <- vector("numeric", 15)
pred <- vector("list", 15)
for (i in (1:length(date_pred))){
  res_arbre <- get_arbre(date_pred[i])
  RMSE[i] <- res_arbre[1]
  MAE[i] <- res_arbre[2]
  pred[[i]] <- res_arbre[3:26]
}
pred <- unlist(pred)



# Random Forest -----------------------------------------------------------
# 
# get_forest <- function(date){
#   index <- which(grepl(pattern = date, prices$timestamp))[1]
#   X <- prices[169:(index-1), c('daynum','prev_day_price','prev_day2_price',
#                                'prev_week_price','Min_price','Max_price',
#                                'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
#   Y <- prices$Zonal_Price[169:(index-1)]
#   r <- randomForest(formula = Y~., data = X, ntree = 100)
#   newX <- prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
#                                      'prev_week_price','Min_price','Max_price',
#                                      'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
#   pred = predict(r, newdata = newX)
#   y <- filter(prices, grepl(pattern = date, prices$timestamp))$Zonal_Price
#   rmse <- get_rmse(y = y, yhat = pred)
#   mae <- get_mae(y = y, yhat = pred)
#   result <- c('rmse' = rmse, 'mae' = mae, 'pred' = pred)
#   return(result)
# }
# 
# 
# #boucle sur les dates a predire
# RMSE <- vector("numeric", 15)
# MAE <- vector("numeric", 15)
# pred <- vector("list", 15)
# for (i in (1:length(date_pred))){
#   res_forest <- get_forest(date_pred[i])
#   RMSE[i] <- res_forest[1]
#   MAE[i] <- res_forest[2]
#   pred[[i]] <- res_forest[3:26]
# }
# pred <- unlist(pred)
# 
# #sauvegarder les resultats dans deux tables
# #TABLE 1 : table des erreurs et force du modele
# #variables : R2_adj, rmse, mae
# #individus : dates a predire
# df_forest_err <- data.frame(date_pred, RMSE, MAE)
# 
# #TABLE 2 : table des predictions
# #variables : dates
# #individus : heures 
# #valeurs : pred
# heures <- 0:23
# df_forest_pred <- data.frame(heures, matrix(pred, nrow = 24))
# colnames(df_forest_pred) <- c('hour', date_pred)
# 
# #on sauvegarde les resultats au format csv
# write_csv(df_forest_err, "data/df_forest_err.csv")
# write_csv(df_forest_pred, "data/df_forest_pred.csv")
# 
# #on sort les resultats
# stargazer(df_forest_err, summary = FALSE)
# stargazer(round(df_forest_pred, 2), summary = FALSE)
# 




#tableau des erreurs des bechmark

df_ridge_err <- read_csv(file = 'data/df_ridge_err.csv',
                         col_types = cols(col_character(), col_double(),
                                          col_double()))
colnames(df_ridge_err) <- c('date_pred', 'Ridge_RMSE', 'Ridge_MAE')

df_lasso_err <- read_csv(file = "data/df_lasso_err.csv",
                         col_types = cols(col_character(), col_double(),
                                          col_double()))
colnames(df_lasso_err) <- c('date_pred', 'Lasso_RMSE', 'Lasso_MAE')

df_benchmark_err <- left_join(df_ridge_err, df_lasso_err)

df_benchmark_err <- df_benchmark_err %>%
  mutate(Ridge_RMSE = round(Ridge_RMSE, 2), Ridge_MAE = round(Ridge_MAE, 2),
         Lasso_RMSE = round(Lasso_RMSE, 2), Lasso_MAE = round(Lasso_MAE, 2)) %>%
  arrange(Ridge_RMSE, Lasso_RMSE, Ridge_MAE, Lasso_RMSE)


stargazer(df_benchmark_err, summary = FALSE, rownames = FALSE)


