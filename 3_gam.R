
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

gam_1 <- gam(formula = Zonal_Price ~ s(daynum,k=7,bs="cc") + s(prev_day_price,k=50,bs="tp") + s(prev_week_price,k=50,bs="tp") +
               s(Min_Price,k=50,bs="tp")+ s(Max_price,k=50,bs="tp")+s(sqrzonalload,k=50,bs="tp")+
               s(cubzonalload,k=100,bs="tp"), data = prices[169:nrow(prices),])

# On crée la variable catégorielle 
prices$daynum=as.integer(factor(prices$day, levels = c("lundi", "mardi", "mercredi","jeudi","vendredi","samedi","dimanche"))
prices$daynum=daynum

names(gam_1)
summary(gam_1)
plot(gam_1)




gam_2 <- gam(formula = Zonal_Price ~ day + s(prev_day_price) + s(prev_week_price) +
               s(Min_Price)+ s(Max_price)+s(sqrzonalload)+
               s(cubzonalload), data = prices[169:nrow(prices),])

plot(gam_2)


summary(gam_2)


