library(tidyverse)
library(zoo)
library(stargazer)
library(broom)
library(MASS)
library(tree)
library(glmnet)
library(gridExtra)

prices <- read_csv(file = "data/tidy_prices.csv")
df_pred <- read_csv(file = "data/df_pred.csv")

date_pred <- c('2013-06-06', '2013-06-17', '2013-06-24', '2013-07-04',
               '2013-07-09', '2013-07-13', '2013-07-16', '2013-07-18',
               '2013-07-19', '2013-07-20', '2013-07-24', '2013-07-25',
               '2013-12-06', '2013-12-07', '2013-12-17')

#resultats des benchmark
df_lasso_err <- read_csv(file = "data/df_lasso_err.csv")
df_lasso_pred <- read_csv(file = "data/df_lasso_pred.csv")

df_ridge_err <- read_csv(file = "data/df_ridge_err.csv")
df_ridge_pred <- read_csv(file = "data/df_ridge_pred.csv")

#resultats des gam
df_gam_err <- read_csv(file = "data/df_gam_err.csv")
df_gam_pred <- read_csv(file = "data/df_gam_pred.csv")


#mise en forme du df

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

tidy_lasso <- df_ridge_pred %>% 
  gather(`2013-06-06`, `2013-06-17`, `2013-06-24`, `2013-07-04`, `2013-07-09`,
         `2013-07-13`, `2013-07-16`, `2013-07-18`, `2013-07-19`, `2013-07-20`,
         `2013-07-24`, `2013-07-25`, `2013-12-06`, `2013-12-07`, `2013-12-17`,
         key = "day", value = "LASSO")


df_pred_plot <- left_join(left_join(left_join(tidy_real, tidy_gam), tidy_ridge), tidy_lasso)

df_pred_plot <- df_pred_plot %>%
  gather(`real_prices`, `GAM`, `Ridge`, `LASSO`,
         key = 'Modele', value = 'prediction') %>%
  mutate(time = paste0(day, ' ', hour, 'h'))


#essai de graph sur tous les jours a predire
ggplot(data = df_pred_plot) +
  geom_line(mapping = aes(x = time, y = prediction, color = Modele, group = Modele)) +
  labs(x = "", y="Prix de l'électicité") +
  scale_colour_discrete(name = '',
                        limits = c("real_prices", "GAM", "Ridge", "LASSO"),
                        labels = c("Prix réels", "GAM", "Ridge", "LASSO")) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()



#fonction qui represente les prix et leurs predictions pour une date donnee
get_plot_pred <- function(date){
  index <- which(df_pred_plot$day == date)
  p <- ggplot(data = df_pred_plot[index,]) +
    geom_line(mapping = aes(x = time, y = prediction, color = Modele, group = Modele)) +
    scale_colour_brewer(name = '',
                        limits = c("real_prices", "GAM", "Ridge", "LASSO"),
                        labels = c("Prix réels", "GAM", "Ridge", "LASSO"),
                        palette = 'Set1') +
    scale_x_discrete(labels = 0:23, breaks = waiver()) +
    theme_bw()
  return(p)
}

get_plot_pred(date)



#graph pour le mois de juin
grid.arrange(get_plot_pred('2013-06-06'), get_plot_pred('2013-06-17'),
             get_plot_pred('2013-06-24'), nrow = 2, ncol = 2)


#graph pour le mois de jullet
grid.arrange(get_plot_pred('2013-07-04'), get_plot_pred('2013-07-09'),
             get_plot_pred('2013-07-13'), get_plot_pred('2013-07-16'),
             get_plot_pred('2013-07-18'), get_plot_pred('2013-07-19'),
             get_plot_pred('2013-07-20'), get_plot_pred('2013-07-24'),
             get_plot_pred('2013-07-25'), nrow = 3, ncol = 3)


#graph pour le mois de decembre
grid.arrange(get_plot_pred('2013-12-06'), get_plot_pred('2013-12-07'),
             get_plot_pred('2013-12-17'), nrow = 2, ncol = 2)
