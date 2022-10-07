rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = F
source("Programme/Simulations_Modele.R")

#############################################
### PARAMETRE FIXES ET DENSITE DEPENDANCE ###
#############################################

# # 1 - Les parametres biologiques fixes :
# parametres.fixes
# 
# # 2 -  Les parametres biologiques densite dependants :
# 
## 2.1 - Reglage par direct des  fonction de competition :
# parametres.dens_dep <- parametres.mobiles$new(const_larve = 0.67, alpha_larve = 0.5,
#                                               const_ponte = 0.99, alpha_ponte = 0.5,
#                                               const_adulte = 0.97, alpha_adulte = 0)
# plot(parametres.dens_dep)
# parametres.dens_dep$type
# 
# ## 2.2 - Reglage par set a meme capacite de charge -> code = 1 a 7 :
# parametres.dens_dep <- parametres.mobiles$new(code = 1)
# plot(parametres.dens_dep)
# parametres.dens_dep$type
# 
# ## 2.3 - Reglage fonction par fonction  -> valeurs = 1 a 4 :
# parametres.dens_dep <- parametres.mobiles$new(survie_adulte = 1, survie_larve = 4, proba_ponte = 4, proba_accoupl = 4)
# plot(parametres.dens_dep)
# parametres.dens_dep$type


##################
### SIMULATION ###
##################

#pop.init <- population$new(parametres.fixes,parametres.dens_dep,vierge=list(nb_couple,5),male=list(nb_couple,5))

pop <- simulation(parametres.fixes[1,],
                  parametres.mobiles$new(alpha_larve = 0.1,alpha_ponte = 0.9),
                  nb_couple_init = 50,
                  quantite_introduite = 25,
                  pourcentage = TRUE,
                  nombre_introduction = 1, 
                  male_ratio = 0,
                  mated_ratio = 1,
                  premiere_introduction = 7, 
                  interval_intro = 14)

#############
### PLOTS ###
#############
pop$description()

plot(pop,description=T)

plot(pop$adultes(vector=T))

plot(pop,pourcentage=T)

plot(pop,ech_log = F)

