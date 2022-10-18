rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")


##################
### SIMULATION ###
##################

table <- parallel.simulation(parametres.fixes,
                             list_of_dens_dep(code = 6:7),
                             nb_couple_init = 50,
                             quantite_introduite = 25,
                             nombre_introduction = 1, 
                             male_ratio = 0, 
                             mated_ratio = 0.5,
                             premiere_introduction = 1:10*10, 
                             interval_intro = 14,
                             pourcentage = T,
                             # synchro = character()
                             repetition = 15)
plot_by_variable(table,premiere_introduction)
plot_by_DD2(table, premiere_introduction, type = 'l')


