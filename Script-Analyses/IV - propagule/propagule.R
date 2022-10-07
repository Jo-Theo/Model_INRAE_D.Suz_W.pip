rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")

nombre_intro <- 1:20
quanti_intro <- 54/nombre_intro

# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = quanti_intro,
#                              nombre_introduction = nombre_intro,
#                              synchro = c("quantite_introduite","nombre_introduction"),
#                              male_ratio = 0,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 7,
#                              interval_intro = 14,
#                              pourcentage = F,
#                              repetition = 300)
# 
# write.csv2(table,"II - Resultats/IV - propagule/propagule_interval_7emeJ_14J.csv")



table <- read.csv2("II - Resultats/IV - propagule/propagule_interval_7emeJ_14J.csv")

plot_by_DD(table, nombre_introduction, split = F, DD_codes = 1:3, type = 'l',
           save=T,name="propagule_7emeJ_14J",path = "II - Resultats/IV - propagule")
plot_by_DD(table, nombre_introduction, split = T, DD_codes = 1:7, type = 'l',
           save=T,name="propagule_7emeJ_14J",path = "II - Resultats/IV - propagule")



# table1 <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code=1:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = quanti_intro,
#                              nombre_introduction = nombre_intro,
#                              synchro = c("quantite_introduite","nombre_introduction"),
#                              male_ratio = 0,
#                              mated_ratio = 0.5,
#                              premiere_introduction = 7,
#                              interval_intro = 7,
#                              pourcentage = F,
#                              repetition = 300)
# 
# write.csv2(table1,"II - Resultats/IV - propagule/propagule_interval_7emeJ_7J.csv")



table1 <- read.csv2("II - Resultats/IV - propagule/propagule_interval_7emeJ_7J.csv")

plot_by_DD(table1, nombre_introduction, split = F, DD_codes = 1:3, type = 'l',
           save=T,name="propagule_7emeJ_7J",path = "II - Resultats/IV - propagule")
plot_by_DD(table1, nombre_introduction, split = T, DD_codes = 1:7, type = 'l',
           save=T,name="propagule_7emeJ_7J",path = "II - Resultats/IV - propagule")




