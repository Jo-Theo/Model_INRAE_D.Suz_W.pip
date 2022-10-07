rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")



## Tres bon set !!!
# alpha_larve <- 0.5
# quantite_introduite <- 25
# male_ratio <- 0
# mated_ratio <- 1
# premiere_introduction <- 7
# nombre_introduction <- 1
## ANALYSE SENSI autour de se set de parametre !!!!!!

  
multi_inv <- function(repetition = 1000,
                      temps_max = 700,
                      alpha_larve = 0.5,
                      quantite_introduite = 25,
                      male_ratio = 0,
                      mated_ratio = 1,
                      premiere_introduction = 7,
                      nombre_introduction = 1){
    list <- list(prop = list(), envahie = logical())
    for (i in seq_len(repetition)){
      pop <- simulation(parametres.fixes[1,],
                        parametres.mobiles$new(alpha_larve = alpha_larve ,alpha_ponte = 1 - alpha_larve),
                        nb_couple_init = 50,
                        quantite_introduite = quantite_introduite,
                        pourcentage = TRUE,
                        nombre_introduction = nombre_introduction, 
                        male_ratio = male_ratio,
                        mated_ratio = mated_ratio,
                        premiere_introduction = premiere_introduction, 
                        interval_intro = 14,
                        summarise_vec = F,
                        temps_max = temps_max)
      list[[1]][[i]] <- pop$proportion(all=T)
      list[[2]] <- c(list[[2]],pop$description()[[1]])
    }
    return(list)
}
detect_inv <- function(proportion, semaine = 30, seuil =  0.74){
    intro <- min(which(proportion!=0))
    prop_init <- proportion[[intro]]
    quantite_introduite <- prop_init/(1-prop_init)
    objectif <- seuil * quantite_introduite
    is.envahie <- any(proportion[intro+seq_len(semaine)*7] >= objectif)
    return(is.envahie)
}
  
# list0 <- multi_inv(temps_max = 700,
#                    quantite_introduite = 25,
#                    premiere_introduction = 21)
# 
# saveRDS(list0,file="list21J.RData")

# list0 <- readRDS(file = 'list7J.RData')
# list1 <- readRDS(file = 'list21J.RData')
# 
# semaines <- 1:15
# seuils <- 0:10/10+0.5
# 
# table0 <- expand.grid(semaines=semaines,seuils=seuils)
# prop <- c()
# faux_neg <- c()
# faux_pos <- c()
# for (i in 1:dim(table0)[[1]]){
#   inv_predict <- as.logical(lapply(list0$prop, function(proportion) detect_inv(proportion,
#                                                                               semaine = table0$semaines[[i]],
#                                                                               seuil =  table0$seuils[[i]])))
#   data <- data.frame(invasion = list0$envahie, prediction = inv_predict)
#   prop <- c(prop,1-length(which(data$invasion != data$prediction))/length(data$invasion))
#   pred_neg <- which(!data$prediction)
#   faux_neg <- c(faux_neg,ifelse(length(pred_neg)==0,0,mean(data$invasion[pred_neg])))
#   pred_pos <- which(data$prediction)
#   faux_pos <- c(faux_pos,ifelse(length(pred_pos)==0,0,mean(!data$invasion[pred_pos])))
# }
# 
# table0$prop <- prop
# table0$faux_neg <- faux_neg
# table0$faux_pos <- faux_pos
# 
# 
# table1 <- expand.grid(semaines=semaines,seuils=seuils)
# prop <- c()
# faux_neg <- c()
# faux_pos <- c()
# for (i in 1:dim(table1)[[1]]){
#   inv_predict <- as.logical(lapply(list1$prop, function(proportion) detect_inv(proportion,
#                                                                               semaine = table1$semaines[[i]],
#                                                                               seuil =  table1$seuils[[i]])))
#   data <- data.frame(invasion = list1$envahie, prediction = inv_predict)
#   prop <- c(prop,1-length(which(data$invasion != data$prediction))/length(data$invasion))
#   pred_neg <- which(!data$prediction)
#   faux_neg <- c(faux_neg,ifelse(length(pred_neg)==0,0,mean(data$invasion[pred_neg])))
#   pred_pos <- which(data$prediction)
#   faux_pos <- c(faux_pos,ifelse(length(pred_pos)==0,0,mean(!data$invasion[pred_pos])))
# }
# 
# table1$prop <- prop
# table1$faux_neg <- faux_neg
# table1$faux_pos <- faux_pos
# 
# write.csv2(table0,"II - Resultats/VIII - invasion detection/table7J.csv")
# write.csv2(table1,"II - Resultats/VIII - invasion detection/table21J.csv")

table0 <- read.csv2("II - Resultats/VIII - invasion detection/table7J.csv")
table1 <- read.csv2("II - Resultats/VIII - invasion detection/table21J.csv")

table <- table1
table_filter <- table %>% filter(!(faux_pos == 0 | faux_neg == 0))
sum <- sqrt(table_filter$faux_neg**2+table_filter$faux_pos**2)
table_filter[which(sum<=min(sum)*1.1),]

