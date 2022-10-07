rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")

nombre_intro <- c(1,1:5*2)
quanti_intro <- 115/nombre_intro

table1 <- read.csv2("II - Resultats/VII - comparatif - temoin/propagule/propagule_interval_7J.csv")
table2 <- read.csv2("II - Resultats/VII - comparatif - temoin/propagule/propagule_interval_14J.csv")


table1 <- table1 %>% filter(nombre_introduction == nombre_intro & quantite_introduite == quanti_intro)
table2 <- table2 %>% filter(nombre_introduction == nombre_intro & quantite_introduite == quanti_intro)


plot_by_DD2(table1, nombre_introduction ,ref = 'Ninf', type = 'l',
            save = T, name = "propagule_7J", path ='II - Resultats/VII - comparatif - temoin/propagule')
plot_by_DD2(table2, nombre_introduction, ref = 'Ninf', type = 'l',
            save = T, name = "propagule_14J", path ='II - Resultats/VII - comparatif - temoin/propagule')
