rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")


# table0 <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = 5,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 0:100,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300)
# 
# write.csv2(table0,"II - Resultats/II - moment_intro/moment_intro_proportion_5.csv")



table0 <- read.csv2("II - Resultats/II - moment_intro/moment_intro_proportion_5.csv")

plot_by_DD(table0, premiere_introduction, split = F, DD_codes = 1:3, type = 'l',
           save=T,name="moment_intro_proportion_5",path = "II - Resultats/II - moment_intro")
plot_by_DD(table0, premiere_introduction, split = T, DD_codes = 1:7, type = 'l',
           save=T,name="moment_intro_proportion_5",path = "II - Resultats/II - moment_intro")



# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = 10,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 0:100,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300)
# 
# write.csv2(table,"II - Resultats/II - moment_intro/moment_intro_proportion_10.csv")



table <- read.csv2("II - Resultats/II - moment_intro/moment_intro_proportion_10.csv")

plot_by_DD(table, premiere_introduction, split = F, DD_codes = 1:3, type = 'l',
           save=T,name="moment_intro_proportion_10",path = "II - Resultats/II - moment_intro")
plot_by_DD(table, premiere_introduction, split = T, DD_codes = 1:7, type = 'l',
           save=T,name="moment_intro_proportion_10",path = "II - Resultats/II - moment_intro")




# table1 <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = 20,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 0:100,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300)
# 
# write.csv2(table1,"II - Resultats/II - moment_intro/moment_intro_proportion_20.csv")



table1 <- read.csv2("II - Resultats/II - moment_intro/moment_intro_proportion_20.csv")

plot_by_DD(table1, premiere_introduction, split = F, DD_codes = 1:3, type = 'l'
           ,save=T,name="moment_intro_proportion_20",path = "II - Resultats/II - moment_intro")
plot_by_DD(table1, premiere_introduction, split = T, DD_codes = 1:7, type = 'l',
           save=T,name="moment_intro_proportion_20",path = "II - Resultats/II - moment_intro")




