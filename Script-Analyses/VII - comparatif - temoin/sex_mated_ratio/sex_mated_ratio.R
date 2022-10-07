rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")



table <- parallel.simulation(parametres.fixes,
                             list_of_dens_dep(code=6:7),
                             nb_couple_init = 50,
                             quantite_introduite = 10,
                             nombre_introduction = 1,
                             male_ratio = 0,
                             mated_ratio = 0:10/10,
                             premiere_introduction = 21,
                             interval_intro = 14,
                             pourcentage = T,
                             repetition = 300)

write.csv2(table,"II - Resultats/VII - comparatif - temoin/mated_ratio.csv")



table <- parallel.simulation(parametres.fixes,
                             list_of_dens_dep(code=6:7),
                             nb_couple_init = 50,
                             quantite_introduite = 10,
                             nombre_introduction = 1,
                             male_ratio = 0:10/10,
                             mated_ratio = 1,
                             premiere_introduction = 21,
                             interval_intro = 14,
                             pourcentage = T,
                             repetition = 300)

write.csv2(table,"II - Resultats/VII - comparatif - temoin/male_ratio.csv")






