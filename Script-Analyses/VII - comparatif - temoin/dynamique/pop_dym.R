rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = F
source("Programme/Simulations_Modele.R")

const_ponte <- 0.99
alpha_ponte <- 0.5

dens_dep <- parametres.mobiles$new(alpha_larve = 1 - alpha_ponte,
                                   alpha_ponte = alpha_ponte,
                                   const_ponte = const_ponte)

quantite_intro <- 25
pourcentage <- T
premiere_intro <- 1
nb_intro <- 1

pop_inf <- simulation(parametres.fixes[1,],
                      dens_dep,
                      nb_couple_init = 50,
                      quantite_introduite = quantite_intro,
                      pourcentage = pourcentage,
                      nombre_introduction = nb_intro, 
                      male_ratio = 0,
                      mated_ratio = 1,
                      premiere_introduction = premiere_intro, 
                      interval_intro = 7,
                      temps_min = 900)
                  

pop_ref <- simulation(parametres.fixes[2,],
                      dens_dep,
                      nb_couple_init = 50,
                      quantite_introduite = quantite_intro,
                      pourcentage = pourcentage,
                      nombre_introduction = nb_intro, 
                      male_ratio = 0,
                      mated_ratio = 1,
                      premiere_introduction = premiere_intro, 
                      interval_intro = 7,
                      temps_min = 900,
                      temps_max = 1000)

ref <- pop_ref$adultes(vector=T)
inf <- pop_inf$adultes(vector=T)



par(mar=c(4,4,3,5)) 
plot(ref,type='l',ylab="Nombre d'adultes",xlab = "Temps (jour)",ylim=c(0,max(ref,inf)),xlim=c(0,1000))
lines(inf,lty=2)
par(new=T)
plot(pop_inf$proportion(all=T),ylim=c(0,1),lty=2,col='red',type='l',ylab = "",xlab="",axes=F,xlim=c(0,1000))
axis(4, ylim=c(0,3), col="red",col.axis="red")
mtext("Pourcentage d'infectes",side=4,col="red",line=2.5)
legend("bottomright",legend=c("Introduction d'infectes","Introduction de sauvages"),lty=c(2,1))


