rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")



table1 <- read.csv2("II - Resultats/VII - comparatif - temoin/sex_mated_ratio/mated_ratio.csv")
table2 <- read.csv2("II - Resultats/VII - comparatif - temoin/sex_mated_ratio/male_ratio.csv")



plot_by_DD2(table1, mated_ratio ,ref = 'Ninf', type = 'l',
            save = T, name = "mated_ratio", path ='II - Resultats/VII - comparatif - temoin/sex_mated_ratio')
plot_by_DD2(table2, male_ratio, ref = 'Ninf', type = 'l',
            save = T, name = "male_ratio", path ='II - Resultats/VII - comparatif - temoin/sex_mated_ratio')
