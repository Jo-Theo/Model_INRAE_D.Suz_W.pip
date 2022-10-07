rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")

nombre_intro <- c(1,1:5*2)
quanti_intro <- 115/nombre_intro

# table <- parallel.simulation(parametres.fixes,
#                              list_of_dens_dep(code=6:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = quanti_intro,
#                              nombre_introduction = nombre_intro,
#                              synchro = c("quantite_introduite","nombre_introduction"),
#                              male_ratio = 0,
#                              mated_ratio = 1,
#                              premiere_introduction = 21,
#                              interval_intro = 7,
#                              pourcentage = F,
#                              repetition = 300)
# 
# write.csv2(table,"II - Resultats/VII - comparatif - temoin/propagule_interval_7J.csv")
# 
# 
# table <- parallel.simulation(parametres.fixes,
#                              list_of_dens_dep(code=6:7),
#                              nb_couple_init = 50,
#                              quantite_introduite = quanti_intro,
#                              nombre_introduction = nombre_intro,
#                              synchro = c("quantite_introduite","nombre_introduction"),
#                              male_ratio = 0,
#                              mated_ratio = 1,
#                              premiere_introduction = 21,
#                              interval_intro = 14,
#                              pourcentage = F,
#                              repetition = 300)
# 
# write.csv2(table,"II - Resultats/VII - comparatif - temoin/propagule_interval_14J.csv")



