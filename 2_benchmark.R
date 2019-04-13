library(tidyverse)
library(zoo)
library(stargazer)
library(broom)
library(MASS)
library(tree)
library(glmnet)

prices <- read_csv(file = "data/tidy_prices.csv")
df_pred <- read_csv(file = "data/df_pred.csv")




# Stationnarisation de la série -------------------------------------------

# Transformation de la série au format ts (time series)
prices_ts <- ts(prices$Zonal_Price, frequency = 365*24) 

# desaisonnaliser
decomposition <- decompose(prices_ts) # on decompose la serie temp
plot(decomposition)
prices_desaiso = prices_ts / decomposition$seasonal
plot(prices_desaiso)

acf(prices_desaiso) # autocorrelations



                         
# Régression Linéaire -----------------------------------------------------

prices2011 <- filter(prices, grepl(pattern = '2011.', timestamp))
prices2012 <- filter(prices, grepl(pattern = '2012.', timestamp))
prices2013 <- filter(prices, grepl(pattern = '2013.', timestamp))
prices$daynum <- daynum

# Préliminaire pour la reg lin et les reg pénalisés :
# Virer les variables NA
prices2011lasso <- na.omit(prices2011)
prices2012lasso <- na.omit(prices2012)
prices2013lasso <- na.omit(prices2013)
prices201213 <- prices[(nrow(prices2011)+1):nrow(prices),]
prices201213lasso <- na.omit(prices201213)

# Première méthode : on utilise la syntaxe des ridge/Lasso avec une pénalisation nulle
modelelineaire <- lm(Zonal_Price ~ daynum+prev_day_price+prev_day2_price +
                       prev_week_price + Min_price + Max_price + Forecasted_Zonal_Load +
                       sqrzonalload + cubzonalload, na.action = na.omit ,
                     data = prices2011[169:nrow(prices),])

ytest <- prices2011lasso$Zonal_Price
ytrain <- prices201213lasso$Zonal_Price

xtrain <- as.matrix(prices201213lasso[,c('daynum','prev_day_price','prev_day2_price',
                                         'prev_week_price','Min_price','Max_price',
                                         'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])
xtest <- as.matrix(prices2011lasso[,c('daynum','prev_day_price','prev_day2_price',
                                      'prev_week_price','Min_price','Max_price',
                                      'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')])

#modlin=lm(ytrain~xtrain,data=prices2011[169:nrow(prices2011),])
modlin <- glmnet(xtrain, ytrain, alpha = 0, lambda = 0)
modlin.pred <- predict(ridge.mod, s = 0, newx = xtest)
sqrt(mean((modlin.pred-ytest)^2))
# On trouve une RMSE = 9,95

# Utilisation de lm standard
modelelineaire <- lm(Zonal_Price ~ daynum + prev_day_price + prev_day2_price +
                       prev_week_price + Min_price + Max_price +
                       Forecasted_Zonal_Load + sqrzonalload +
                       cubzonalload, na.action = na.omit,
                     data=prices[169:nrow(prices),])

summary(modelelineaire)
coef(modelelineaire)
fitted(modelelineaire)
residuals(modelelineaire)

# Sortie Latex avec stargazer
modlin_tidy <- tidy(modelelineaire)
stargazer(modlin_tidy, type = 'latex', title = "Résultats de la régression linéaire",
          summary=FALSE)

plot(modelelineaire)



### Ridge ###
#############

# Les xtrain,test ... sont définis plus haut

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(xtrain, ytrain, alpha <- 0, lambda = grid)
ridge.pred <- predict(ridge.mod, s = 4, newx = xtest)
sqrt(mean((ridge.pred-ytest)^2))

# Tuning du paramètre lambda
set.seed(1)
cv.out <- cv.glmnet(xtrain, ytrain, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = xtest)
sqrt(mean((ridge.pred-ytest)^2))
out <- glmnet(x = xtest, y = ytest, alpha = 0)

ridgesortant <- predict(out, type = "coefficients", s <- bestlam)
ridge_tidy <- tidy(ridgesortant)
stargazer(ridge_tidy, type = 'latex', title = 'Résultats du Ridge - Lambda optimal',
          summary=FALSE)


### Estimateur LASSO ###
########################

lasso.mod = glmnet(xtrain, ytrain, alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(xtrain, ytrain, alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s = bestlam, newx = xtest)
sqrt(mean((lasso.pred-ytest)^2))
out1 = glmnet(x = xtest, y = ytest, alpha = 1)
ridgesortant = predict(out, type = "coefficients", s = bestlam)
ridge_tidy = tidy(ridgesortant)
stargazer(ridge_tidy, type = 'latex', title = 'Résultats du Lasso - Lambda optimal',
          summary=FALSE)

### Arbre aléatoire ###
#######################


set.seed(1)
ytest = prices2011lasso$Zonal_Price
ytrain = prices201213lasso$Zonal_Price
xtrain = prices201213lasso[,c('daynum','prev_day_price','prev_day2_price',
                              'prev_week_price','Min_price','Max_price',
                              'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]
xtest = prices2011lasso[,c('daynum','prev_day_price','prev_day2_price',
                           'prev_week_price','Min_price','Max_price',
                           'Forecasted_Zonal_Load','sqrzonalload','cubzonalload')]

arbre = tree(formula = ytrain~., data = xtrain)
summary(arbre)
plot(arbre)
text(arbre,pretty=0)
cv.arbre = cv.tree(arbre)
plot(cv.arbre$size, cv.arbre$dev, type = 'b')
prune.arbre = prune.tree(arbre, best = 10)
plot(prune.arbre)
text(prune.arbre)
text(prune.arbre, pretty = 0)
abline(0,1)
yhat = predict(arbre, newdata = xtest)
sqrt(mean((yhat-ytest)^2))
# RMSE de 13,64

### Random Forest : 
library(randomForest)
r=randomForest(formula= ytrain~.,data=xtrain,ntree=200)
plot(r)
yhatrf=predict(r,newdata=xtest)
sqrt(mean((yhatrf-ytest)^2)) # RMSE à 10 en gros ...
importance(r)
varImpPlot(r)



### AR ###
##########

#cf article Serinaldi

AR.1 <- lm(Zonal_Price ~ prev_day_price + prev_day2_price + prev_week_price +
             Min_price + day_lundi + day_samedi + day_dimanche,
           data = prices[169:nrow(prices),])


summary(AR.1)




