
library(tidyverse)

prices <- read_csv(file = "data/tidy_prices.csv")




# ANALYSE BIVARIEE --------------------------------------------------------



#### variables continues de demande locale et totale

ggplot(prices) +
  geom_point(mapping = aes(x = Forecasted_Total_Load, y = Forecasted_Zonal_Load))

# relation lineaire entre les variables de demande

#### variables de demande en fonction du prix

ggplot(prices) +
  geom_point(mapping = aes(x = Zonal_Price, y = Forecasted_Total_Load))

ggplot(prices) +
  geom_point(mapping = aes(x = Zonal_Price, y = Forecasted_Zonal_Load))




#### variables de prix et de date

# sur la serie complete (toutes les dates)

ggplot(prices) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))



# par annee
prices2011 <- filter(prices, grepl(pattern = '2011.', timestamp))
prices2012 <- filter(prices, grepl(pattern = '2012.', timestamp))
prices2013 <- filter(prices, grepl(pattern = '2013.', timestamp))

# Création dataframe5novembre:
prices5nov2011<-filter(prices,grepl(pattern='2011-11-05.',timestamp))
prices5nov2012<-filter(prices,grepl(pattern='2012-11-05.',timestamp))
prices5nov2013<-filter(prices,grepl(pattern='2013-11-05.',timestamp))
prices5dec2013<-filter(prices,grepl(pattern='2013-12-05.',timestamp)) # pour identifier un lundi et dimanche

# Création dataframe d'un dimanche et d'un lundi de décembre pour comparer
prices8dec2013<-filter(prices,grepl(pattern='2013-12-08.',timestamp)) # un dimanche
prices9dec2013<-filter(prices,grepl(pattern='2013-12-09.',timestamp)) # un lundi
pricesdimlunddec<-c(prices8dec2013,prices9dec2013)
pdldec=rbind(prices8dec2013,prices9dec2013)



ggplot(prices2011) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))
ggplot(prices2012) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))
ggplot(prices2013) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))


# par semaine
prices_week1 <- prices[1:168,]
ggplot(prices_week1) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))

prices_week7 <- prices[1000:1168,]
ggplot(prices_week7) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))


#### Suite analyse bivariée : 
#############################


ggplot(prices)+
  geom_point(mapping = aes(x = Zonal_Price, y = prev_day_price))

ggplot(prices)+
  geom_point(mapping = aes(x = Zonal_Price, y = prev_week_price))

ggplot(prices)+
  geom_point(mapping = aes(x = prev_week_price, y = prev_day_price))




#### La petite analyse du bon Jerem : 
#####################################



# Evolution du prix sur toute la période : 
ggplot(prices)+
  geom_point(mapping =aes(x = timestamp, y = Zonal_Price))

# Evolution du prix pour chaque année :
années=list(prices2011,prices2012,prices2013)
for(k in années){
  g=ggplot(k)+
    geom_point(mapping=aes(x=timestamp,y=Zonal_Price))
  g
}

# Evolution de la consommation pour chaque année : 
ggplot(prices2013)+
  geom_point(mapping=aes(x=timestamp,y=Forecasted_Zonal_Load))

# Prix et prévision de consommation :
#####################################

ggplot(prices2013,fill=Zonal_Price)+
  geom_point(mapping=aes(x=timestamp,y=Zonal_Price),colour='green')+
  geom_point(mapping=aes(x=timestamp,y=Forecasted_Zonal_Load/120),colour='red')
# Pour avoir les autres années, on remplace le 2012

# Scalage à la main (je tatonne sur le diviseur pour obtenir la même moyenne entre 
# le prix et la prévision de consommation --> donc un diviseur propre à chaque année) 
summary(prices2013$Zonal_Price)
summary(prices2013$Forecasted_Zonal_Load/120)

# Comparaison de prix au 5 novembre (arbitraire) : 
##################################################
ggplot(prices5nov2013)+
  geom_line(mapping=aes(x=timestamp,y=Zonal_Price,colour='green'),colour='red')
# Changer le 2013 pour les années

# Comparaison d'un dimanche et lundi : 
ggplot(pdldec)+
  geom_line(mapping=aes(x=timestamp,y=Zonal_Price),colour='red')


ggplot(data = pdldec) +
  geom_line(mapping = aes(x = timestamp[1:24], y = Zonal_Price[1:24]))

pdldec_d <- pdldec[1:24,]
pdldec_l <- pdldec[25:48,]

pdldec_d$Type = "Dimanche"
pdldec_l$Type = "Lundi"

df1 = data.frame(Value = c(2,3,4,5),
                 Frequency = c(1,7,19,9),
                 Percentage = c(2.77,19.44,52.77,25))
df2 = data.frame(Value = c(1,2,3,4,5),
                 Frequency = c(2,3,8,20,20),
                 Percentage = c(3.77,5.66,15.09,37.73,37.73))

df1$Type = "A"
df2$Type = "B"

dfw = rbind(df1,df2)

ggplot(data = dfw, aes(x = Value, y = Percentage, color = Type)) + geom_line()





# test modif
# test modif 2

