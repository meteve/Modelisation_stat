
library(tidyverse)
library(lubridate)
library(RColorBrewer)

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
a <- ggplot(prices)+
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price))+
  ggtitle("Evolution du prix sur la période")

a + theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)


# Evolution du prix pour chaque année :
années=list(prices2011,prices2012,prices2013)
for(k in années){
  g=ggplot(k)+
    geom_point(mapping=aes(x=timestamp,y=Zonal_Price))
  g
}

d<-ggplot(prices2013)+
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price))+
  ggtitle("Evolution du prix sur la période 2013")

d + theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)



# Evolution de la consommation pour chaque année : 
g<-ggplot(prices2012)+
  geom_line(mapping=aes(x=timestamp,y=Forecasted_Zonal_Load))+
  ggtitle("Evolution de la consommation sur la période 2013")

g + theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)
a





# Prix et prévision de consommation :
#####################################

h<-ggplot(prices2011,fill=Zonal_Price)+
  geom_line(mapping=aes(x=timestamp,y=Zonal_Price),colour='green')+
  geom_line(mapping=aes(x=timestamp,y=Forecasted_Zonal_Load/120),colour='red')+
  ggtitle("Comparaison Prix/Consommation 2011")

h+theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)

date <- rep(c(prices2011$timestamp), 2)
type <- c(rep("price", nrow(prices2011)), rep("zonal_load", nrow(prices2011)))
values <- c(prices2011$Zonal_Price, (prices2011$Forecasted_Zonal_Load)/120)

df_graph_2011 <- tibble(values, type, date)

ggplot(df_graph_2011) +
  geom_line(mapping = aes(x = date, y = values, color = type)) +
  scale_colour_brewer(name = "Coucou",
                    palette = "Dark2")




summary(prices2011$Zonal_Price)
summary(prices2011$Forecasted_Zonal_Load/120)

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

ggplot(data = pdldec) +
  geom_line(mapping = aes(x = hour, y = Zonal_Price, color = day)) +
  scale_fill_brewer(name = "Opinion concernant\nl'état de l'envrionnement",
                    palette = "Dark2")






# test modif
# test modif 2

