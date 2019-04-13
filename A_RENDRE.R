# Les résultats des modèles ont été exportés lorsque nous avons fait tourner notre code.
# Ces résultats sont donc ici seulement importés et les boucles qui estiment les modèles
# ont été mises en commentaires pour que le code s'exécute rapidement.


# Les packages nécessaires
library(tidyverse)
library(lubridate)
library(fastDummies)
library(plyr)

library(RColorBrewer)
library(gridExtra)
library(grid)
library(lemon)
library(ggpubr)

library(glmnet)
library(rpart)
library(mgcv)

library(stargazer)







# Plan du code :
# PARTIE 0 : Nettoyage de la base et creation de variables
# PARTIE 1 : Statistiques descriptives et plot
# PARTIE 2 : Modèles de benchmark
# PARTIE 3 : Modèles GAM
# PARTIE 4 : Comparaisons GAM / Benchmark




##################################################################################
############# PARTIE 0 : NETTOYAGE  DE LA BASE ET CREATION VARIABLES #############
##################################################################################


# IMPORTATION DES DONNEES -------------------------------------------------

# importer en convertissant directement en format 'datetime' la deuxieme colonne
prices <- read_delim("data/prices.csv", delim = ";",
                     col_types = cols('timestamp' = 
                                        col_datetime("%m%d%Y %H:%M")))

# on remplace les espaces par des underscore dans les noms de colonnes
prices <- dplyr::rename(prices, ZONEID = ZONEID, timestamp = timestamp,
                        Forecasted_Total_Load = `Forecasted Total Load`,
                        Forecasted_Zonal_Load = `Forecasted Zonal Load`,
                        Zonal_Price = `Zonal Price`)



# CREATION DE VARIABLES ---------------------------------------------------

############## DUMMIES

# dummies pour l'annee
prices$year <- year(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'year')

# dummies pour le mois
prices$month <- month(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'month')

# dummies pour le jour
prices <- prices %>%
  mutate(day = weekdays(as.Date(timestamp)))
prices$day <- tolower(prices$day)
prices <- dummy_cols(prices, select_columns = 'day')

# dummies pour le week-end 
prices$samedi<-as.numeric(prices$day_samedi==1)
prices$dimanche<-as.numeric(prices$day_dimanche==1)
prices$weekend<-(prices$samedi + prices$dimanche)

# dummies pour l'heure
prices$hour <- hour(prices$timestamp)
prices <- dummy_cols(prices, select_columns = 'hour')

####### AUTRES

# prix a la journee precedente 
previous_day_price <- apply(as.matrix(25:nrow(prices)), 1,
                            function(i){prices$Zonal_Price[(i-24)]})
previous_day_price_NA <- rep(NA, 24)
prices$prev_day_price <- c(previous_day_price_NA, previous_day_price)

# prix à deux jours avant 
previous_day2_price <- apply(as.matrix(49:nrow(prices)), 1,
                             function(i){prices$Zonal_Price[(i-48)]})
previous_day2_price_NA <- rep(NA, 48)
prices$prev_day2_price <- c(previous_day2_price_NA, previous_day2_price)

# prix a la semaine precedente
previous_week_price <- apply(as.matrix(169:nrow(prices)), 1,
                             function(i){prices$Zonal_Price[(i-168)]})
previous_week_price_NA <- rep(NA, 168)
prices$prev_week_price <- c(previous_week_price_NA, previous_week_price)

# prix minimum des 24h précédentes
Min_price <- apply(as.matrix(25:nrow(prices)), 1,
                   function(i){min(prices$Zonal_Price[(i-24):i])})
Min_price_NA <- rep(NA, 24)
prices$Min_price <- c(Min_price_NA, Min_price)

# prix maximum des 24h précédentes
Max_price <- apply(as.matrix(25:nrow(prices)), 1,
                   function(i){max(prices$Zonal_Price[(i-24):i])})
Max_price_NA<-rep(NA,24)
prices$Max_price<-c(Max_price_NA, Max_price)

# load au carré (Total Load)
prices$sqrtotalload <- (prices$Forecasted_Total_Load)^2
# load au carré (Zonal Load)
prices$sqrzonalload <- (prices$Forecasted_Zonal_Load)^2
# load au cube (Total Load)
prices$cubtotalload <- (prices$Forecasted_Total_Load)^3
# load au cube (Zonal Load)
prices$cubzonalload <- (prices$Forecasted_Zonal_Load)^3

# Association d'une valeur numérique à chaque jour de la semaine
prices$daynum <- as.integer(factor(prices$day,
                                   levels = c("lundi", "mardi", "mercredi",
                                              "jeudi","vendredi","samedi","dimanche")))


# on cree une table de donnees pour les jours a predire
# fonction qui retourne pour une date donnee la table des variables a chaque heure
get_df_pred <- function(x){
  df_pred <- filter(prices, grepl(pattern = x, timestamp))
  return(df_pred)
}

#dates a predire
date_pred <- c('2013-06-06', '2013-06-17', '2013-06-24', '2013-07-04',
               '2013-07-09', '2013-07-13', '2013-07-16', '2013-07-18',
               '2013-07-19', '2013-07-20', '2013-07-24', '2013-07-25',
               '2013-12-06', '2013-12-07', '2013-12-17')

#liste des tables de donnees pour chaque date a predire
list_df_pred <- lapply(date_pred, get_df_pred)
#dataframe des dates a predire
df_pred <- ldply(list_df_pred)
head(df_pred)

#on exporte les donnees nettoyees
write_csv(df_pred, "data/df_pred.csv")
write_csv(prices, "data/tidy_prices.csv")






##################################################################################
###################### PARTIE 1 : STAT DESCRIPTIVES ET PLOT ######################
##################################################################################

#on importe les donnees nettoyees
prices <- read_csv(file = "data/tidy_prices.csv")
df_pred <- read_csv(file = "data/df_pred.csv")


# Analyse univariee du prix -----------------------------------------------

#### SUR TOUTE LA PERIODE
plot_prices_total <- ggplot(data = prices) +
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price)) +
  theme(
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14)) +
  xlab("") + ylab("Prix dans la zone étudiée")

plot_prices_total


### PAR MOIS
plot_price_month <- function(date){
  index <- which(prices$timestamp == date)
  prices_month <- prices[index:(index+720),]
  p <- ggplot(prices_month) +
    geom_line(mapping = aes(x = timestamp, y = Zonal_Price)) +
    xlab("") + ylab("Prix dans la zone étudiée")
  return(p)
}

plot_price_month('2011-01-02')

### PAR SEMAINE
# Fonction qui plot les prix pour une semaine donnée
# prend comme argument la date d'un jour et plot les prix sur la semaine
plot_price_week <- function(date){
  index <- which(prices$timestamp == date)
  prices_week <- prices[index:(index+168),]
  p <- ggplot(prices_week) +
    geom_line(mapping = aes(x = timestamp, y = Zonal_Price)) +
    xlab("") + ylab("Prix dans la zone étudiée")
  return(p)
}

#plot des premieres semaines de chaque mois de 2013
grid.arrange(plot_price_week('2013-01-07'), plot_price_week('2013-03-04'),
             plot_price_week('2013-05-06'), plot_price_week('2013-07-01'),
             plot_price_week('2013-09-02'), plot_price_week('2013-11-04'),
             nrow = 3, ncol = 2)

### PAR JOUR
plot_price_day <- function(date){
  index <- which(prices$timestamp == date) +1
  prices_day <- prices[index:(index+23),]
  p <- ggplot(prices_day) +
    geom_line(mapping = aes(x = timestamp, y = Zonal_Price)) +
    ylab("Prix dans la zone étudiée") + xlab("")
  return(p)
}

#plot de 6 mardis de 2013
grid.arrange(plot_price_day('2013-01-08'), plot_price_day('2013-03-05'),
             plot_price_day('2013-05-07'), plot_price_day('2013-07-02'),
             plot_price_day('2013-09-03'), plot_price_day('2013-11-05'),
             nrow = 3, ncol = 2)



# Analyse bivariee prix et consommation -----------------------------------

# prix et prevision de la consommation

# on cree le tibble adapte pour les graphiques de superposition de la demande et du prix
date <- rep(c(prices$timestamp), 2)
type <- c(rep("price", nrow(prices)), rep("zonal_load", nrow(prices)))

# on centre et reduit les vecteurs de prix et de consommation pour les comparer
Zonal_Price_sd = (prices$Zonal_Price - mean(prices$Zonal_Price))/sd(prices$Zonal_Price)
Forecasted_Zonal_Load_sd = (prices$Forecasted_Zonal_Load - mean(prices$Forecasted_Zonal_Load))/sd(prices$Forecasted_Zonal_Load)

values <- c(Zonal_Price_sd, Forecasted_Zonal_Load_sd)
df_graph <- tibble(date, type, values)

# graph des prix et de la conso sur toute la periode
ggplot(df_graph) +
  geom_line(mapping = aes(x = date, y = values, color = type), alpha = 0.5) +
  scale_color_manual(labels = c("Prix dans la zone",
                                "Consommation prédite\ndans la zone"),
                     values = c("black", "deepskyblue"),
                     name = "") +
  theme_bw() +
  xlab("") + ylab("Prix et consommation prédite (valeurs centrées réduites)")





##################################################################################
######################## PARTIE 2 : MODELES DE BENCHMARK #########################
##################################################################################


#fonctions de calcul des erreurs
get_rmse <- function(y, yhat){
  rmse <- sqrt(mean((y-yhat)^2))
  return(rmse)
}
get_mae <- function(y,yhat){
  mae <- mean(abs(y-yhat))
  return(mae)
}



# Ridge -------------------------------------------------------------------

# fonction qui fit un ridge pour une date en question,
# qui predit et retourne les predictions et erreurs
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

#on importe les resultats (pour ne pas encore faire tourner la boucle)
df_ridge_err <- read_csv(file = 'data/df_ridge_err.csv',
                         col_types = cols(col_character(), col_double(), col_double()))
colnames(df_ridge_err) <- c('date_pred', 'Ridge_RMSE', 'Ridge_MAE')



# LASSO -------------------------------------------------------------------

# fonction qui fit un LASSO pour une date en question,
# qui predit et retourne les predictions et erreurs
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

#on importe les resultats (pour ne pas encore faire tourner la boucle)
df_lasso_err <- read_csv(file = "data/df_lasso_err.csv",
                         col_types = cols(col_character(), col_double(), col_double()))
colnames(df_lasso_err) <- c('date_pred', 'Lasso_RMSE', 'Lasso_MAE')



# Arbre -------------------------------------------------------------------

# fonction qui fit un arbre de regression pour une date en question,
# qui predit et retourne les predictions et erreurs
get_arbre <- function(date){
  index <- which(grepl(pattern = date, prices$timestamp))[1]
  X <- prices[169:(index-1), c('daynum','prev_day_price','prev_day2_price',
                               'prev_week_price','Min_price','Max_price',
                               'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
  Y <- prices$Zonal_Price[169:(index-1)]
  arbre = rpart(formula = Y ~., data = X, method = 'anova')
  newX <- prices[index:(index+23), c('daynum','prev_day_price','prev_day2_price',
                                     'prev_week_price','Min_price','Max_price',
                                     'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
  pred <- predict(arbre, s = 10, newdata = newX)
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
  res_arbre <- get_arbre(date_pred[i])
  RMSE[i] <- res_arbre[1]
  MAE[i] <- res_arbre[2]
  pred[[i]] <- res_arbre[3:26]
}
pred <- unlist(pred)


#sauvegarder les resultats dans deux tables
#TABLE 1 : table des erreurs et force du modele
#variables : R2_adj, rmse, mae
#individus : dates a predire
df_tree_err <- data.frame(date_pred, RMSE, MAE)

#TABLE 2 : table des predictions
#variables : dates
#individus : heures
#valeurs : pred
heures <- 0:23
df_tree_pred <- data.frame(heures, matrix(pred, nrow = 24))
colnames(df_tree_pred) <- c('hour', date_pred)

#on sauvegarde les resultats au format csv
write_csv(df_tree_err, "data/df_tree_err.csv")
write_csv(df_tree_pred, "data/df_tree_pred.csv")

#on importe les resultats (pour ne pas encore faire tourner la boucle)
df_tree_err <- read_csv(file = "data/df_tree_err.csv",
                        col_types = cols(col_character(), col_double(), col_double()))
colnames(df_tree_err) <- c('date_pred', 'Arbre_RMSE', 'Arbre_MAE')




# Mise en forme  et synthese des résultats des Benchmark -------------------------------
df_benchmark_err <- left_join(left_join(df_ridge_err, df_lasso_err), df_tree_err) %>%
  mutate(Ridge_RMSE = round(Ridge_RMSE, 2), Ridge_MAE = round(Ridge_MAE, 2),
         Lasso_RMSE = round(Lasso_RMSE, 2), Lasso_MAE = round(Lasso_MAE, 2),
         Arbre_RMSE = round(Arbre_RMSE, 2), Arbre_MAE = round(Arbre_MAE, 2))

df_benchmark_err <- dplyr::select(df_benchmark_err, date_pred, Ridge_RMSE,
                                  Lasso_RMSE, Arbre_RMSE, Ridge_MAE, Lasso_MAE, Arbre_MAE)

# sortie des resultats en format latex
stargazer(df_benchmark_err, summary = FALSE, rownames = FALSE)






##################################################################################
############################# PARTIE 3 : MODELES GAM #############################
##################################################################################


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
#on repasse aux valeur exponentielles
df_gam_pred[,2:16] <- exp(df_gam_pred[,2:16])


#on sauvegarde les resultats au format csv
write_csv(df_gam_err, "data/df_gam_err.csv")
write_csv(df_gam_pred, "data/df_gam_pred.csv")

#on importe les résultats
df_gam_err <- read_csv(file = "data/df_gam_err.csv",
                       col_types = cols(col_character(), col_double(), col_double(), col_double()))
df_gam_pred <- read_csv(file = "data/df_gam_pred.csv")

#on sort les resultats en format latex
stargazer(df_gam_err, summary = FALSE, rownames = FALSE)
stargazer(round(df_gam_pred, 2), summary = FALSE, rownames = FALSE)



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






##################################################################################
#################### PARTIE 4 : COMPARAISONS GAM / BENCHMARK #####################
##################################################################################

# on réimporte les resultats des benchmark et des gam
df_lasso_err <- read_csv(file = "data/df_lasso_err.csv")
df_lasso_pred <- read_csv(file = "data/df_lasso_pred.csv")

df_ridge_err <- read_csv(file = "data/df_ridge_err.csv")
df_ridge_pred <- read_csv(file = "data/df_ridge_pred.csv")

df_tree_err <- read_csv(file = "data/df_tree_err.csv")
df_tree_pred <- read_csv(file = "data/df_tree_pred.csv")

df_gam_err <- read_csv(file = "data/df_gam_err.csv")
df_gam_pred <- read_csv(file = "data/df_gam_pred.csv")

#mise en forme du dataframe pour ggplot
real_prices <- df_pred$Zonal_Price
hour <- rep(0:23, 15)
day <- rep(date_pred, each = 24)

tidy_real <- tibble(hour, day, real_prices)

tidy_gam <- df_gam_pred %>% 
  gather(`2013-06-06`, `2013-06-17`, `2013-06-24`, `2013-07-04`, `2013-07-09`,
         `2013-07-13`, `2013-07-16`, `2013-07-18`, `2013-07-19`, `2013-07-20`,
         `2013-07-24`, `2013-07-25`, `2013-12-06`, `2013-12-07`, `2013-12-17`,
         key = "day", value = "GAM")

tidy_ridge <- df_ridge_pred %>% 
  gather(`2013-06-06`, `2013-06-17`, `2013-06-24`, `2013-07-04`, `2013-07-09`,
         `2013-07-13`, `2013-07-16`, `2013-07-18`, `2013-07-19`, `2013-07-20`,
         `2013-07-24`, `2013-07-25`, `2013-12-06`, `2013-12-07`, `2013-12-17`,
         key = "day", value = "Ridge")

tidy_lasso <- df_lasso_pred %>% 
  gather(`2013-06-06`, `2013-06-17`, `2013-06-24`, `2013-07-04`, `2013-07-09`,
         `2013-07-13`, `2013-07-16`, `2013-07-18`, `2013-07-19`, `2013-07-20`,
         `2013-07-24`, `2013-07-25`, `2013-12-06`, `2013-12-07`, `2013-12-17`,
         key = "day", value = "LASSO")

tidy_tree <- df_tree_pred %>% 
  gather(`2013-06-06`, `2013-06-17`, `2013-06-24`, `2013-07-04`, `2013-07-09`,
         `2013-07-13`, `2013-07-16`, `2013-07-18`, `2013-07-19`, `2013-07-20`,
         `2013-07-24`, `2013-07-25`, `2013-12-06`, `2013-12-07`, `2013-12-17`,
         key = "day", value = "Arbre")


# on ne represente pas les predictions avec l'arbre de regression car celles ci ne sont pas continues
df_pred_plot <- left_join(left_join(left_join(tidy_real, tidy_gam),
                                    tidy_ridge), tidy_lasso)

df_pred_plot <- df_pred_plot %>%
  gather(`real_prices`, `GAM`, `Ridge`, `LASSO`,
         key = 'Modele', value = 'prediction') %>%
  mutate(time = paste0(day, ' ', hour, 'h'))


# fonction qui représente les prix et leurs predictions pour une date donnée
get_plot_pred <- function(date){
  index <- which(df_pred_plot$day == date)
  p <- ggplot(data = df_pred_plot[index,]) +
    geom_line(mapping = aes(x = time, y = prediction, color = Modele, group = Modele)) +
    scale_colour_brewer(name = '',
                        limits = c("real_prices", "GAM", "Ridge", "LASSO"),
                        labels = c("Prix réels", "GAM", "Ridge", "LASSO"),
                        palette = 'Set1') +
    xlab(date) +
    ylab('Prix') +
    scale_x_discrete(labels = 0:23, breaks = waiver()) +
    theme(panel.background = element_blank(), legend.background = element_blank())
  return(p)
}


#graph des predictions pour le mois de juin
grid_arrange_shared_legend(get_plot_pred('2013-06-06'), get_plot_pred('2013-06-17'),
                           get_plot_pred('2013-06-24'), nrow = 2, ncol = 2)

#graph des predictions pour le mois de jullet
grid_arrange_shared_legend(get_plot_pred('2013-07-04'), get_plot_pred('2013-07-09'),
                           get_plot_pred('2013-07-13'), get_plot_pred('2013-07-16'),
                           get_plot_pred('2013-07-18'), get_plot_pred('2013-07-19'),
                           get_plot_pred('2013-07-20'), get_plot_pred('2013-07-24'),
                           get_plot_pred('2013-07-25'), nrow = 3, ncol = 3)

#graph des predictions pour le mois de decembre
grid_arrange_shared_legend(get_plot_pred('2013-12-06'), get_plot_pred('2013-12-07'),
                           get_plot_pred('2013-12-17'), nrow = 2, ncol = 2)
