
######################
### CONSTRUCTION : ###
######################

# rm(list=ls(all.names=T))
# 
# # TRUE ou FALSE selon le model desire
# stochastique = F
# source("Programme/Simulations_Modele.R")
# 
# 
# code <- expand.grid(alpha_larve = 1:100/100,
#                     alpha_ponte = 1:100/100)
# 
# list_of_eq <- rep(NA,nrow(code))
# 
# for (i in seq_len(nrow(code))){
#   pop <- simulation(parametres.fixes[1,],
#                     list_of_dens_dep(code[i,])[[1]],
#                     nb_couple_init = 50,
#                     quantite_introduite = 0,
#                     pourcentage = F,
#                     nombre_introduction = 1,
#                     male_ratio = 0,
#                     mated_ratio = 1,
#                     premiere_introduction = 100,
#                     summarise_vec = F)
#   list_of_eq[[i]] <- pop$adultes()[1]
# }
# 
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
#                              DD_by_types = F)
# save(code,file='code.RData')
# save(list_of_eq,file='list_of_eq.RData')
# save(table,file='table.RData')


##########################
### FLITER ET SAUVER : ###
##########################

# rm(list=ls(all.names=T))
# 
# load("table.RData")
# load("code.RData")
# load("list_of_eq.RData")
# data <- cbind(code,table,equilibre = list_of_eq)
# 
# write.csv2(data, "II - Resultats/V - densite_dep_larves_pontes/densite_dep_3D_10%_7eme_jour.csv")
# 
# file.remove("table.RData")
# file.remove("code.RData")
# file.remove("list_of_eq.RData")

###############
### PLOTS : ###
###############

# rm(list=ls(all.names=T))
# library("plotly")
# data <- read.csv2("II - Resultats/V - densite_dep_larves_pontes/densite_dep_3D_10%_7eme_jour/densite_dep_3D_10%_7eme_jour.csv")
# 
# 
# Names_of_variables <- c("equilibre","envahie","temps_50","diminution_relative")
# 
# nom <- Names_of_variables[[1]]
# 
# if (nom == 'equilibre'){
#   data[which(data$equilibre>2000),] <- NA
# }
# 
# alpha_larve <- as.numeric(levels(factor(data$alpha_larve)))
# alpha_ponte <- as.numeric(levels(factor(data$alpha_ponte)))
# dim1 <- length(alpha_larve)
# dim2 <- length(alpha_ponte)
# 
# grille <- data[,c(1:4)]
# 
# y.nappe <- with(grille, data[nom]*1)[[1]]
# y.mat <- matrix(y.nappe, nrow=dim2, ncol=dim1, byrow=T)
# 
# 
# # Mise en forme
# main = nom
# font <- list(family = "Courier New, monospace",size = 12,color = "#7f7f7f")
# xlabel <- list(title = paste("competition larve"," (x)"),titlefont = font)
# ylabel <- list(title = paste("competition ponte"," (y)"),titlefont = font)
# zlabel <- list(title = paste(main," (z)"),titlefont = font)
# scene = list(xaxis = xlabel ,yaxis = ylabel ,zaxis = zlabel)
# 
# 
# plot_3D <- plot_ly(x = alpha_larve, y = alpha_ponte , z = y.mat) %>%
#   layout(title = main, scene = scene)  %>% add_surface() #%>% add_trace(x = x1, y = x2, z = y, mode = 'markers')
# plot_3D
# 
# htmlwidgets::saveWidget(as_widget(plot_3D),
#                         paste0("densite_dep_3D_10%_7eme_jour_",nom,".html"))
