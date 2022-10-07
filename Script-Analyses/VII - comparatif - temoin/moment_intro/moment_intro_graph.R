rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")



table1 <- read.csv2("II - Resultats/VII - comparatif - temoin/moment_introduction/moment_intro_proportion.csv")
table2 <- read.csv2("II - Resultats/VII - comparatif - temoin/moment_introduction/moment_intro_proportion_eq.csv")



plot_by_DD2(table1, premiere_introduction ,ref = 'Ninf', type = 'l',
            save = T, name = "moment_intro", path ='II - Resultats/VII - comparatif - temoin/moment_introduction')
plot_by_DD2(table2, premiere_introduction, ref = 'Ninf', type = 'l',
            save = T, name = "moment_intro_eq", path ='II - Resultats/VII - comparatif - temoin/moment_introduction')
