rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")




table0 <- parallel.simulation(parametres.fixes,
                              list_of_dens_dep(code=6:7),
                              nb_couple_init = 50,
                              quantite_introduite = 10,
                              nombre_introduction = 1,
                              male_ratio = 0,
                              mated_ratio = 1,
                              premiere_introduction = c(1,1:10*10),
                              interval_intro = 14,
                              pourcentage = T,
                              repetition = 300)

write.csv2(table0,"II - Resultats/VII - comparatif - temoin/moment_intro_proportion.csv")

table1 <- parallel.simulation(parametres.fixes,
                              list_of_dens_dep(code=6:7),
                              nb_couple_init = 50,
                              quantite_introduite = 115,
                              nombre_introduction = 1,
                              male_ratio = 0,
                              mated_ratio = 1,
                              premiere_introduction = c(1,1:10*10),
                              interval_intro = 14,
                              pourcentage = F,
                              repetition = 300)

write.csv2(table1,"II - Resultats/VII - comparatif - temoin/moment_intro_proportion_eq.csv")



