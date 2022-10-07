rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")


# 
# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = 10,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 0:100/100,
#                              premiere_introduction = 100,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300)
# 
# write.csv2(table,"II - Resultats/III - sex_mated_ratio/mated_ratio.csv")



table <- read.csv2("II - Resultats/III - sex_mated_ratio/mated_ratio.csv")

plot_by_DD(table, mated_ratio, split = F, DD_codes = 1:3, type = 'l')
plot_by_DD(table, mated_ratio, split = T, DD_codes = 1:7, type = 'l')




# table1 <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = 10,
#                              nombre_introduction = 1,
#                              male_ratio = 0:100/100,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 100,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300)
# 
# write.csv2(table1,"II - Resultats/III - sex_mated_ratio/male_ratio.csv")



table1 <- read.csv2("II - Resultats/III - sex_mated_ratio/male_ratio.csv")

plot_by_DD(table1, male_ratio, split = F, DD_codes = 1:3, type = 'l')
plot_by_DD(table1, male_ratio, split = T, DD_codes = 1:7, type = 'l')




