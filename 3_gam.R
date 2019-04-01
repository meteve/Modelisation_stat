
library(tidyverse)
library(mgcv)


prices <- read_csv(file = "data/tidy_prices.csv")


# Identification d'autocorrélations et d'autocorrélations partielles ------------

# Rappel : l'autocorrélation retire l'influence de X_(t-h) sur X_(t)
# et l'autorcorrélation partielle retire la même influence une fois 
# les effet de X_(t-h+1) retirés

acf(prices$Zonal_Price,lag.max=1680)
pacf(prices$Zonal_Price,lag.max=10)

# GAM ---------------------------------------------------------------------

# Topo sur la syntaxe : 
# La forme générale peut être constatée dans le modèle ci dessous
# s est une fonction, on peut paramétrer bs : le type de spline, k le nombre de noeuds
# plot(gam) donne des plots pour les effets NON LINEAIRES

# On crée la variable catégorielle 
#prices$daynum = as.integer(factor(prices$day, levels = c("lundi", "mardi", "mercredi","jeudi","vendredi","samedi","dimanche")))
                        
gam_1 <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(prev_day_price,k=50,bs="tp") + s(prev_week_price,k=50,bs="tp") +
               s(Min_Price,k=50,bs="tp")+ s(Max_price,k=50,bs="tp")+s(sqrzonalload,k=50,bs="tp")+
               s(Forecasted_Zonal_Load) +s(cubzonalload,k=100,bs="tp"), data = prices[169:nrow(prices),])

names(gam_1)
summary(gam_1)
plot(gam_1)


prices$numyear <- as.factor(prices$year)
levels(prices$numyear) <- c('1', '2', '3')
prices$numyear <- as.numeric(prices$numyear)
table(prices$numyear)


#on ajoute des variables
gam_2 <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(month,k=12,bs='cc') +
               s(prev_day_price,k=50,bs="tp") + s(hour, k=24, bs='cc') +
               s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp")+
               s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") + 
               s(Forecasted_Zonal_Load) + s(cubzonalload,k=100,bs="tp") +
               s(Forecasted_Total_Load) + s(sqrtotalload,k=50,bs="tp") +
               s(cubtotalload,k=100,bs="tp"), 
             data = prices[169:nrow(prices),])


summary(gam_2)
gam.check(gam_2)

# en changeant les degres de liberte
gam_3 <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(month,k=12,bs='cc') +
               s(prev_day_price,k=50,bs="tp") + s(hour, k=24, bs='cc') +
               s(prev_week_price,k=50,bs="tp") + s(Min_Price,k=50,bs="tp")+
               s(Max_price,k=50,bs="tp") + s(sqrzonalload,k=50,bs="tp") + 
               s(Forecasted_Zonal_Load, bs="tp") + s(cubzonalload,k=100,bs="tp") +
               s(Forecasted_Total_Load, bs="tp") + s(sqrtotalload,k=50,bs="tp") +
               s(cubtotalload,k=100,bs="tp"), 
             data = prices[169:nrow(prices),])

summary(gam_3)

gam.check(gam_3)


#creer le vecteur des prix a predire

prev <- predict(gam_3, newprices)






