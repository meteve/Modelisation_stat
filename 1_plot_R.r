
library(tidyverse)
library(lubridate)
library(RColorBrewer)

prices <- read_csv(file = "data/tidy_prices.csv")

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



# par semaine
prices_week1 <- prices[1:168,]
ggplot(prices_week1) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))

prices_week7 <- prices[1000:1168,]
ggplot(prices_week7) +
  geom_line(mapping = aes(x = timestamp, y = Zonal_Price))




# Prix en fonction des prix passés

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

# 2011
date <- rep(c(prices2011$timestamp), 2)
type <- c(rep("price", nrow(prices2011)), rep("zonal_load", nrow(prices2011)))
values <- c(prices2011$Zonal_Price, (prices2011$Forecasted_Zonal_Load)/120)

df_graph_2011 <- tibble(values, type, date)

i<-ggplot(df_graph_2011) +
  geom_line(mapping = aes(x = date, y = values, color = type)) +
  ggtitle("Prix et consommation 2011")+
  scale_colour_brewer(name = "Légende",
                    palette = "Dark2")

i+theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)

# 2012
date <- rep(c(prices2012$timestamp), 2)
type <- c(rep("price", nrow(prices2012)), rep("zonal_load", nrow(prices2012)))
values <- c(prices2012$Zonal_Price, (prices2012$Forecasted_Zonal_Load)/150)

df_graph_2012 <- tibble(values, type, date)

j<-ggplot(df_graph_2012) +
  geom_line(mapping = aes(x = date, y = values, color = type)) +
  ggtitle("Prix et consommation 2012")+
  scale_colour_brewer(name = "Légende",
                      palette = "Dark2")

j+theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)


# 2013
date <- rep(c(prices2013$timestamp), 2)
type <- c(rep("price", nrow(prices2013)), rep("zonal_load", nrow(prices2013)))
values <- c(prices2013$Zonal_Price, (prices2013$Forecasted_Zonal_Load)/120)

df_graph_2013 <- tibble(values, type, date)

j<-ggplot(df_graph_2013) +
  geom_line(mapping = aes(x = date, y = values, color = type)) +
  ggtitle("Prix et consommation 2013")+
  scale_colour_brewer(name = "Légende",
                      palette = "Dark2")

j+theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)


# Pour avoir les autres années, on remplace le 2012

# Scalage à la main (je tatonne sur le diviseur pour obtenir la même moyenne entre 
# le prix et la prévision de consommation --> donc un diviseur propre à chaque année) 
summary(prices2013$Zonal_Price)
summary(prices2013$Forecasted_Zonal_Load/120)

# Comparaison de prix au 5 novembre (arbitraire) : 
##################################################
# Création du dataframe pour les 3 "5novembre"
hour5<-rep(c(prices5nov2011$hour),3)
type5<-c(rep("2011",nrow(prices5nov2011)),rep("2012",nrow(prices5nov2012)),rep("2013",nrow(prices5nov2013)))
values5<-c(prices5nov2011$Zonal_Price,prices5nov2012$Zonal_Price,prices5nov2013$Zonal_Price)
df5nov<-tibble(values5,type5,hour5)

k<-ggplot(df5nov)+
  geom_line(mapping=aes(x=hour5,y=values5,color=type5))+
  ggtitle("Prix au 5 novembre")+
  scale_colour_brewer(name="Légende",palette="Dark2")
k
j<-ggplot(df_graph_2012) +
  geom_line(mapping = aes(x = date, y = values, color = type)) +
  ggtitle("Prix et consommation 2012")+
  scale_colour_brewer(name = "Légende",
                      palette = "Dark2")




# Comparaison d'un dimanche et lundi : 

ggplot(data = pdldec) +
  geom_line(mapping = aes(x = hour, y = Zonal_Price, color = day)) +
  scale_fill_brewer(name = "Opinion concernant\nl'état de l'envrionnement",
                    palette = "Dark2")

# Comparaison des prix sur les 3 années





# test modif
# test modif 2

