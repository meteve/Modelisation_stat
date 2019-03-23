
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

prices <- read_csv(file = "data/tidy_prices.csv")






# Analyse univariee du prix -----------------------------------------------



# SUR TOUTE LA PERIODE 

plot_prices_total <- ggplot(data = prices) +
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price)) +
  theme(
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)) +
  xlab("") + ylab("Prix dans la zone étudiée")

plot_prices_total




# PAR ANNEE
prices2011 <- filter(prices, grepl(pattern = '2011.', timestamp))
prices2012 <- filter(prices, grepl(pattern = '2012.', timestamp))
prices2013 <- filter(prices, grepl(pattern = '2013.', timestamp))


ggplot(data = prices2011) +
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price)) +
  xlab("") + ylab("Prix dans la zone étudiée")

ggplot(data = prices2012) +
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price)) +
  xlab("") + ylab("Prix dans la zone étudiée")

ggplot(data = prices2013) +
  geom_line(mapping =aes(x = timestamp, y = Zonal_Price)) +
  xlab("") + ylab("Prix dans la zone étudiée")


# PAR MOIS

plot_price_month <- function(date){
  index <- which(prices$timestamp == date)
  prices_month <- prices[index:(index+720),]
  p <- ggplot(prices_month) +
    geom_line(mapping = aes(x = timestamp, y = Zonal_Price)) +
    xlab("") + ylab("Prix dans la zone étudiée")
  return(p)
}


plot_price_month('2011-01-02')
plot_price_month('2011-03-01')
plot_price_month('2011-06-02')
plot_price_month('2011-07-02')
plot_price_month('2011-08-02')







# PAR SEMAINE

# fonction qui plot les prix pour une semaine donnee
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








# PAR JOUR

plot_price_day <- function(date){
  index <- which(prices$timestamp == date) +1
  prices_day <- prices[index:(index+23),]
  p <- ggplot(prices_day) +
    geom_line(mapping = aes(x = timestamp, y = Zonal_Price)) +
    ylab("Prix dans la zone étudiée") + xlab("")
  return(p)
}

plot_price_day('2013-01-08')

grid.arrange(plot_price_day('2013-01-08'), plot_price_day('2013-03-05'),
             plot_price_day('2013-05-07'), plot_price_day('2013-07-02'),
             plot_price_day('2013-09-03'), plot_price_day('2013-11-05'),
             nrow = 3, ncol = 2)

scale_x_discrete(name = date, labels = c('00:00', '01:00', '02:00',
                                         '03:00', '04:00', '05:00',
                                         '06:00', '07:00', '08:00',
                                         '09:00', '10:00', '11:00',
                                         '12:00', '13:00', '14:00',
                                         '15:00', '16:00', '17:00',
                                         '18:00', '19:00', '20:00',
                                         '21:00', '22:00', '23:00'),
                 breaks = c(prices_day$timestamp))


# Création dataframe d'un dimanche et d'un lundi de décembre pour comparer
prices8dec2013 <- filter(prices,grepl(pattern='2013-12-08.',timestamp)) # un dimanche
prices9dec2013 <- filter(prices,grepl(pattern='2013-12-09.',timestamp)) # un lundi
pricesdimlunddec <- c(prices8dec2013,prices9dec2013)
pdldec=rbind(prices8dec2013,prices9dec2013)



# Comparaison d'un dimanche et lundi : 

ggplot(data = pdldec) +
  geom_line(mapping = aes(x = hour, y = Zonal_Price, color = day)) +
  scale_fill_brewer(name = "Opinion concernant\nl'état de l'envrionnement",
                    palette = "Dark2")



# Prix en fonction des prix passés

ggplot(prices)+
  geom_point(mapping = aes(x = Zonal_Price, y = prev_day_price))

ggplot(prices)+
  geom_point(mapping = aes(x = Zonal_Price, y = prev_week_price))

ggplot(prices)+
  geom_point(mapping = aes(x = prev_week_price, y = prev_day_price))






# Analyse bivariee prix et consommation -----------------------------------


# prix et prevision de la consommation

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











# Création dataframe5novembre:
prices5nov2011 <- filter(prices,grepl(pattern='2011-11-05.',timestamp))
prices5nov2012 <- filter(prices,grepl(pattern='2012-11-05.',timestamp))
prices5nov2013 <- filter(prices,grepl(pattern='2013-11-05.',timestamp))
prices5dec2013 <- filter(prices,grepl(pattern='2013-12-05.',timestamp)) # pour identifier un lundi et dimanche



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










#### La petite analyse du bon Jerem : 
#####################################






# Evolution de la consommation pour chaque année : 
g<-ggplot(prices2012)+
  geom_line(mapping=aes(x=timestamp,y=Forecasted_Zonal_Load))+
  ggtitle("Evolution de la consommation sur la période 2013")

g + theme(
  plot.title = element_text(color="black", size=14,hjust=0.5),
  axis.title.x = element_text(color="black", size=14),
  axis.title.y = element_text(color="black", size=14)
)



