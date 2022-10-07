
########################
### CONSTRUCTION 1 : ###
########################

# rm(list=ls(all.names=T))
# 
# # TRUE ou FALSE selon le model desire
# stochastique = T
# source("Programme/Simulations_Modele.R")
# 
# 
# list <- 1:99/100
# 
# code <- data.frame(const_adulte = list)
# 
# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code),
#                              nb_couple_init = 50,
#                              quantite_introduite = 10,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 1,
#                              premiere_introduction = 7,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300,
#                              DD_by_types = F)
# 
# long_code <- code[table$id_cas,]
# data <- cbind(long_code,table)
# names(data)[[1]] <- names(code)
# 
# write.csv2(data, paste0("II - Resultats/effet_",names(code),".csv"))


########################
### CONSTRUCTION 2 : ###
########################

# rm(list=ls(all.names=T))
# 
# # TRUE ou FALSE selon le model desire
# stochastique = T
# source("Programme/Simulations_Modele.R")
# 
# 
# list <- 1:99/100
# 
# code <- data.frame(const_larve = list)
# 
# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code),
#                              nb_couple_init = 50,
#                              quantite_introduite = 10,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 1,
#                              premiere_introduction = 7,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300,
#                              DD_by_types = F)
# 
# long_code <- code[table$id_cas,]
# data <- cbind(long_code,table)
# names(data)[[1]] <- names(code)
# 
# write.csv2(data, paste0("II - Resultats/effet_",names(code),".csv"))

########################
### CONSTRUCTION 3 : ###
########################

# rm(list=ls(all.names=T))
# 
# # TRUE ou FALSE selon le model desire
# stochastique = T
# source("Programme/Simulations_Modele.R")
# 
# 
# list <- 1:99/100
# 
# code <- data.frame(const_ponte = list)
# 
# table <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code),
#                              nb_couple_init = 50,
#                              quantite_introduite = 10,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 1,
#                              premiere_introduction = 7,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              repetition = 300,
#                              DD_by_types = F)
# 
# long_code <- code[table$id_cas,]
# data <- cbind(long_code,table)
# names(data)[[1]] <- names(code)
# 
# write.csv2(data, paste0("II - Resultats/effet_",names(code),".csv"))

