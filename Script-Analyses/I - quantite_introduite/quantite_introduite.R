rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")



# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = 0:60/2,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 100,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300)
# 
# write.csv2(table,"II - Resultats/I - quantite_introduite/new1.csv")



table <- read.csv2("II - Resultats/I - quantite_introduite/quantite_introduite_100emeJours.csv")

plot_by_DD(table, quantite_introduite, split = F, DD_codes = 1:3, type = 'l')
plot_by_DD(table, quantite_introduite, split = T, DD_codes = 1:7, type = 'l')



# table1 <- parallel.simulation(parametres.fixes[1,],
#                               list_of_dens_dep(code=1:7),
#                               nb_couple_init = 50,
#                               quantite_introduite = (0:60/2)*5.4, # 5.4 = 1% introduit par rapport a la situation d'equilibre
#                               nombre_introduction = 1,
#                               male_ratio = 0,
#                               mated_ratio = 0.5,
#                               premiere_introduction = 7,
#                               interval_intro = 14,
#                               pourcentage = F,
#                               repetition = 300)
# 
# write.csv2(table1,"II - Resultats/I - quantite_introduite/new2.csv")



table1 <- read.csv2("II - Resultats/I - quantite_introduite/quantite_introduite_7emeJours.csv")

plot_by_DD(table1, quantite_introduite, split = F, DD_codes = 1:3, type = 'l')
plot_by_DD(table1, quantite_introduite, split = T, DD_codes = 1:7, type = 'l')



