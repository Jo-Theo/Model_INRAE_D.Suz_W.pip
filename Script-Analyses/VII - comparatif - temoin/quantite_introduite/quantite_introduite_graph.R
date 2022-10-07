rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")



table1 <- read.csv2("II - Resultats/VII - comparatif - temoin/quantite_introduite/quantite_introduite_21emeJours.csv")



plot_by_DD2(table1, quantite_introduite ,ref = 'Ninf', type = 'l',
            save = T, name = "quantite_introduite", path ='II - Resultats/VII - comparatif - temoin/quantite_introduite')
