
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
# code1 <- expand.grid(alpha_larve = 1:100/100,
#                     alpha_ponte = 1:100/100)
# 
# 
# table1 <- parallel.simulation(parametres.fixes[1,],
#                              list_of_dens_dep(code1),
#                              nb_couple_init = 50,
#                              quantite_introduite = 20,
#                              nombre_introduction = 1,
#                              male_ratio = 0,
#                              mated_ratio = 1,
#                              premiere_introduction = 7,
#                              interval_intro = 14,
#                              pourcentage = T,
#                              DD_by_types = F)
# save(code1,file='code1.RData')
# save(table1,file='table1.RData')


##########################
### FLITER ET SAUVER : ###
##########################

# rm(list=ls(all.names=T))
# 
# load("table1.RData")
# load("code1.RData")
# data1 <- cbind(code1,table1)
# 
# write.csv2(data1, "II - Resultats/V - densite_dep_larves_pontes/densite_dep_3D_20%_7eme_jour/densite_dep_3D_20%_7eme_jour.csv")
# 
# file.remove("table1.RData")
# file.remove("code1.RData")


###############
### PLOTS : ###
###############

# rm(list=ls(all.names=T))
# library("plotly")
# data <- read.csv2("II - Resultats/V - densite_dep_larves_pontes/densite_dep_3D_20%_7eme_jour/densite_dep_3D_20%_7eme_jour.csv")
# 
# Names_of_variables <- c("envahie","temps_50","diminution_relative")
# 
# nom <- Names_of_variables[[1]]
# 
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
#                         paste0("densite_dep_3D_20%_7eme_jour_",nom,".html"))
