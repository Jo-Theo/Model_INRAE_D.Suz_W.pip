################
### PACKAGES ###
################

InstalledPackage <- function(package){
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
  return(available)
}

## Packages necessaires
libraries <- c("R6","zoo","parallel","foreach","doParallel","dplyr","ggplot2","cowplot")
available <- InstalledPackage(libraries)

if(!all(available)){
  stop(paste0("\n\nOH LA LA !!!\nYou need first to install the packages : ",
             paste(libraries[!available],collapse = ', '),
             '\nCopy in the consol this command : install.packages(c("',
             paste(libraries[!available],collapse = '","'),'"))'))
}

for(library in libraries){
  suppressMessages(suppressWarnings(require(library)))
}


########################
### CHARGEMENT MODEL ###
########################

##  Boucle qui selectionne le model a charger, si la variable stochastique n'est pas definie
if(!exists('stochastique') || !is.logical(stochastique)){
  model <- 0
  cat("\nModele stochastique ou deterministe ?\n")
  while(!model %in% 1:2){
    cat("[1] Modele stochastique : 1\n[2] Modele deterministe : 2\n")
    model <- as.numeric(scan(nmax=1))
  }
  stochastique <- model==1
  rm(list = 'model')
}
## Chargement reel du model
source(paste0("Programme/Class_Modele",ifelse(stochastique,"_","_no_"),"var.R"))
cat("\nImportation Modele",ifelse(stochastique,"stochastique","deterministe"),": ")
source("Programme/Parametrage.R")
source("Programme/Plot.R")
cat("Reussie\n")

############################
### FONCTION SECONDAIRES ###
############################

.describe_time <- function(time_in_seconds){
  units <- c('jour','heure','minute','seconde')
  nb_second <- floor(time_in_seconds)
  if(nb_second == 0){
    return(paste(round(time_in_seconds,3), 'secondes'))
  }else{
    nb_min <- nb_second %/% 60
    if(nb_min != 0){
      nb_second <- nb_second %% 60
      nb_hour <- nb_min %/% 60
      if(nb_hour != 0){
        nb_min <- nb_min %% 60
        nb_days <- nb_hour %/% 24
        if(nb_days != 0){
          nb_hour <- nb_hour %% 24
        }
      }else{
        nb_days <- 0
      }
    }else{
      nb_days <- 0
      nb_hour <- 0
    }
    list_temps <- c(nb_days,nb_hour,nb_min,nb_second)
    unit_max <- min(which(list_temps != 0))
    unit_min <- min(unit_max+1,4)
    if(list_temps[unit_min]==0){unit_min <- unit_max}
    list_temps <- list_temps[unit_max:unit_min]
    units <- units[unit_max:unit_min]
    list <- paste(list_temps,paste0(units,ifelse(list_temps>1,'s','')))
    return(paste(list,collapse = ' et '))
  }
}

###########################
### INTRODUCTION VECTOR ###
###########################

# Attribut la classe "introduction.vector" a une liste de format particulier
# Permet d'associer un summary particulier
.as.introduction.vector <- function(obj){
  if(is.list(obj) && all(names(obj) == c('male','femelle','vierge')) && is.numeric(obj$male) && is.numeric(obj$femelle) &&
     is.numeric(obj$vierge)){
    class(obj) <- c('introduction.vector','list')
  }else{
    stop("obj should be a list of 3 numerical vector (same length) called 'male', 'femelle' and 'vierge'")
  }
  return(obj)
}

# Resume les informations principales du vecteur d'introduction
summary.introduction.vector <- function(obj, pourcentage = T){
  nb_male <- sum(obj$male!=0)
  nb_femelle <- sum(obj$femelle!=0 | obj$vierge!=0)
  tot_male <- sum(obj$male)
  tot_femelle_accouplee <- sum(obj$femelle)
  tot_femelle <- sum(obj$femelle + obj$vierge)
  list_male <- which(obj$male!=0)
  list_femelle <- which(obj$femelle!=0)
  if(nb_male>0 & nb_femelle>0){
    if(all(list_male==list_femelle)){
      cat("\n-----  MALES : -----\n\n")
      cat("Nombre d'introduction : ",nb_male,'\n')
      cat("Total introduit : ",tot_male,ifelse(pourcentage,"%"," individus"),"\n")
      cat("Moyenne des introductions : ",tot_male/nb_male,ifelse(pourcentage,"%"," individus"),'\n\n')
      cat("-----  FEMALES : -----\n\n")
      cat("Nombre d'introduction : ",nb_femelle,'\n')
      cat("Total introduit : ",tot_femelle,ifelse(pourcentage,"%"," individus"),"\n")
      cat("Moyenne des introductions : ",tot_femelle/nb_femelle,ifelse(pourcentage,"%"," individus"),'\n\n')
      cat("Sex-ratio moyen (males/total) : ",tot_male/(tot_femelle+tot_male)*100,"%\n")
      cat("Mated-ratio moyen (femelle-accouplee/total femelle) : ",tot_femelle_accouplee/tot_femelle*100,"%\n")
      cat(ifelse(nb_male==1,"Lachers le", "Lachers les"),list_male,ifelse(nb_male==1,"ieme jours", "ieme jours"),"\n\n")
    }else{
      cat("\n-----  MALES : -----\n\n")
      cat("Nombre d'introduction : ",nb_male,'\n')
      cat("Total introduit : ",tot_male,ifelse(pourcentage,"%"," individus"),"\n")
      cat("Moyenne des introductions : ",tot_male/nb_male,ifelse(pourcentage,"%"," individus"),'\n')
      cat(ifelse(nb_male==1,"Lachers le", "Lachers les"),list_male,ifelse(nb_male==1,"ieme jours", "ieme jours"),"\n\n")
      cat("-----  FEMALES : -----\n\n")
      cat("Nombre d'introduction : ",nb_femelle,'\n')
      cat("Total introduit : ",tot_femelle,ifelse(pourcentage,"%"," individus"),"\n")
      cat("Moyenne des introductions : ",tot_femelle/nb_femelle,ifelse(pourcentage,"%"," individus"),'\n')
      cat("Mated-ratio moyen (femelle-accouplee/total femelle) : ",tot_femelle_accouplee/tot_femelle*100,"%\n")
      cat(ifelse(nb_femelle==1,"Lachers le", "Lachers les"),list_femelle,ifelse(nb_femelle==1,"ieme jours", "ieme jours"),'\n\n')
      cat("Sex-ratio moyen (males/total) : ",tot_male/(tot_femelle+tot_male)*100,"%\n")
    }
  }else if(nb_femelle>0){
    cat("\n-----  Lachers de femelles uniquement : -----\n\n")
    cat("Nombre d'introduction : ",nb_femelle,'\n')
    cat("Total introduit : ",tot_femelle,ifelse(pourcentage,"%"," individus"),"\n")
    cat("Moyenne des introductions : ",tot_femelle/nb_femelle,ifelse(pourcentage,"%"," individus"),'\n')
    cat("Mated-ratio moyen (femelle-accouplee/total femelle) : ",tot_femelle_accouplee/tot_femelle*100,"%\n")
    cat(ifelse(nb_femelle==1,"Lachers le", "Lachers les"),list_femelle,ifelse(nb_femelle==1,"ieme jours", "ieme jours"),"\n\n")
  }else if(nb_male>0){
    cat("\n-----  Lachers de males uniquement : -----\n\n")
    cat("Nombre d'introduction :",nb_male,'\n')
    cat("Total introduit :",tot_male,ifelse(pourcentage,"%"," individus"),"\n")
    cat("Moyenne des introductions :",tot_male/nb_male,ifelse(pourcentage,"%"," individus"),'\n')
    cat(ifelse(nb_male==1,"Lachers le", "Lachers les"),list_male,ifelse(nb_male==1,"ieme jours", "ieme jours"),"\n\n")
  }else{
    cat("\nAucun lachers prevus\n\n")
  }
}


# Fabrique une liste de deux vecteurs un pour les introduction des males et un pour les femelles
vector_of_intro <- function(quantite_introduite = 10, nombre_introduction = 1, 
                            male_ratio = 0, mated_ratio = 0.5, premiere_introduction = 100, interval_intro = 14,vec.init=NULL){
  if(is.null(vec.init)){
    if(nombre_introduction != 0){
      list <- (0:(nombre_introduction - 1))*interval_intro + premiere_introduction
      vec.init <- rep(0,list[[length(list)]])
      vec.init[list] <- 1
    }else{
      vec.init  <- 0
    }
  }
  vec <- list(male = vec.init*quantite_introduite*male_ratio,
              femelle = vec.init*quantite_introduite*(1-male_ratio)*mated_ratio,
              vierge = vec.init*quantite_introduite*(1-male_ratio)*(1-mated_ratio))
  return(.as.introduction.vector(vec))
}

###############################
### FIN INTRODUCTION VECTOR ###
###############################

###############################################
### DEFINITION DES FONCTIONS DE SIMULATIONS ###
###############################################



## SIMULATION UNIQUE :
simulation <- function(parametres.fixes,
                       parametres.dens_dep,
                       nb_couple_init = 50,
                       quantite_introduite = 10,
                       pourcentage = T,
                       nombre_introduction = 1, 
                       male_ratio = 0,
                       mated_ratio = 0.5,
                       premiere_introduction = 100, 
                       interval_intro = 14,
                       vec.init = NULL,
                       population0 = NULL,
                       summarise_vec = T,
                       temps_min = NULL,
                       temps_max = NULL){
  ## On genere le vecteur ressencent les inoculations
  vector_of_intro <- vector_of_intro(quantite_introduite = quantite_introduite, nombre_introduction = nombre_introduction, 
                                     male_ratio = male_ratio, mated_ratio = mated_ratio, premiere_introduction = premiere_introduction, 
                                     interval_intro = interval_intro, vec.init = vec.init)
  if(summarise_vec){
    summary(vector_of_intro,pourcentage = pourcentage)
  }
  
  ## On creer la population
  if(class(population0)[[1]]=="population"){
    pop <- population0$clone(deep=T)
  }else{
    pop <- population$new(parametres.fixes,parametres.dens_dep,vierge=nb_couple_init,male=nb_couple_init)
  }
  ## on simule jusqu'a la prochaine centaine
  for(i in 1:(100*(1+trunc(pop$t/100))-pop$t)){
    pop$step(vector_of_intro, pourcentage)
  }
  sup <- 0
  inf <- 0
  # Quel est la derniere introduction
  last_intro <- max(as.numeric(lapply(vector_of_intro,function(obj) return(ifelse(all(obj==0),0,max(which(obj!=0)))))))
  temps_min <- ifelse(is.null(temps_min),last_intro,max(last_intro,temps_min))
  ## debut de la boucle, on ne s'arrete qu'une fois l'equilibre atteind 
  while(sup <= 2 & inf <= 3){
    for(i in 1:100){
      pop$step(vector_of_intro, pourcentage)
    }
    if(temps_min < pop$t){
      if(pop$proportion() > 0.8){
        sup <- sup + 1
      }else{
        sup <- 0
      }
      if(pop$adultes()[[2]] < 1){
        inf <- inf + 1
      }else{
        inf <- 0
      }
      if(!is.null(temps_max) && pop$t >= temps_max){
        return(pop)
      }
    }
  }
  return(pop)
}




## SIMULATION EN SERIE :
multi.simulation <- function(parametres.fixes,
                             parametres.dens_dep,
                             nb_couple_init = 50,
                             quantite_introduite = 10,
                             nombre_introduction = 1, 
                             male_ratio = 0,
                             mated_ratio = 1,
                             premiere_introduction = 100, 
                             interval_intro = 14,
                             synchro = character(),
                             pourcentage = T,
                             repetition = 1,
                             check = F,
                             DD_by_types = T,
                             ref = "Ninf",
                             temps_ref = 1000){
  debut_tot <- Sys.time()
  cat('Working ...\nCould take a while')
  var_names <- c("set_parametre","set_densite_dep",
                 "nb_couple_init","quantite_introduite",
                 "nombre_introduction","male_ratio","mated_ratio",
                 "premiere_introduction","interval_intro")
  if(any(! synchro %in% var_names[-c(1,2)])){
    stop("synchro comprend des noms de variables frauduleux")
  }
  nb_lignes <- sapply(synchro, function(name) length(eval(parse(text=name))))
  if(length(nb_lignes) != 0 && any(nb_lignes != nb_lignes[[1]])){
    stop("synchro contain variable names of different lengths")
  }
  parametres_numeric <- expand.grid(nb_couple_init = nb_couple_init,
                                    quantite_introduite = quantite_introduite,
                                    nombre_introduction = nombre_introduction, 
                                    male_ratio = male_ratio,
                                    mated_ratio = mated_ratio,
                                    premiere_introduction = premiere_introduction, 
                                    interval_intro = interval_intro)
  if(length(nb_lignes) != 0){
    lines <-  T
    for(i in synchro){
      lines <- lines & parametres_numeric[[i]] == eval(parse(text=i))
    }
    parametres_numeric <- parametres_numeric[which(lines),]
  }
  parametres_numeric <- parametres_numeric[rep(seq_len(dim(parametres_numeric)[[1]]), 
                                               each = dim(parametres.fixes)[[1]]*length(parametres.dens_dep)),]
  nr <- dim(parametres_numeric)[1]
  
  ## Liste des arguments
  arg <- list(parametres.fixes,
              parametres.dens_dep,
              parametres_numeric$nb_couple_init,
              parametres_numeric$quantite_introduite,
              parametres_numeric$nombre_introduction, 
              parametres_numeric$male_ratio,
              parametres_numeric$mated_ratio,
              parametres_numeric$premiere_introduction, 
              parametres_numeric$interval_intro)
  names(arg) <- var_names
  nb_col <- length(var_names)
  ## Transforme en listes les agruments non numeriques
  ### Parametres fixes
  id_PF <- rownames(arg[[1]])
  arg[[1]] <- split(arg[[1]], seq(nrow(arg[[1]])))
  
  ### Parametres densite_dep
  if(class(arg[[2]])[[1]]!='list'){
    arg[[2]] <- list(arg[[2]])
  }
  ## Nommes les set de densite dep 
  if(!is.null(names(arg[[2]]))){
    id_DD <- names(arg[[2]])
  }else if(DD_by_types){
    id_DD <- c()
    for(i in arg[[2]]){
      id_DD <- c(id_DD, paste(i$type, collapse = '_'))
    }
  }else{
    id_DD <- seq_len(length(arg[[2]]))
  }
  arg[[1]] <- arg[[1]][rep(rep(seq_len(length(arg[[1]])),length(id_DD)),
                           nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep)))]
  arg[[2]] <- arg[[2]][rep(rep(seq_len(length(arg[[2]])),each=length(id_PF)),
                           nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep)))]
  
  ## Donnee d'entree de simulation
  data_init <- data.frame(matrix(NA,nr*repetition,nb_col+1))
  names(data_init) <- c("id_cas",var_names)
  for (i in seq_len(nb_col)){
    ## Remplie le tableau d'entree
    if(i==1){
      data_init[[i+1]] <- rep(rep(rep(id_PF,length(id_DD)),
                              nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep))),each=repetition)
    }else if(i==2){
      data_init[[i+1]] <- rep(rep(rep(id_DD,each = length(id_PF)),
                              nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep))),each=repetition)
    }else{
      data_init[[i+1]] <- unlist(arg[[i]][rep(seq_len(nr),each=repetition)])
    }
  }
  data_init$id_cas <- rep(seq_len(nr), each = repetition)
  ## Tableau de sortie de simulation
  data_res <- data.frame(matrix(NA,nr*repetition,3))
  names(data_res) <- c("envahie", "temps_50", "diminution_relative")
  list_temps <- rep(0,nr*repetition)
  for (i in seq_len(nr)){
    for (j in seq_len(repetition)){
      is.ref <- data_init$set_parametre[[(i-1)*repetition+j]] %in% ref
      if(is.ref){
        temps_max <- temps_ref
      }else{
        temps_max <- NULL
      }
      debut_simul <- Sys.time()
      pop <- simulation(parametres.fixes = arg$set_parametre[[i]],
                        parametres.dens_dep = arg$set_densite_dep[[i]],
                        nb_couple_init = arg$nb_couple_init[[i]],
                        quantite_introduite = arg$quantite_introduite[[i]],
                        nombre_introduction = arg$nombre_introduction[[i]], 
                        male_ratio = arg$male_ratio[[i]],
                        mated_ratio = arg$mated_ratio[[i]],
                        premiere_introduction = arg$premiere_introduction[[i]], 
                        interval_intro = arg$interval_intro[[i]],
                        pourcentage = pourcentage,
                        summarise_vec = F,
                        temps_max = temps_max)
      fin_simul <- Sys.time()
      list_temps[[(i-1)*repetition+j]] <- as.numeric(difftime(time1 = fin_simul, time2 = debut_simul, units = "secs"))
      if(check){
        pop$plot(description=T)
      }
      ## Remplie le tableau de sortie
      data_res[(i-1)*repetition+j,] <- pop$description(forced = is.ref)
    }
  }
  fin_tot <- Sys.time()
  temps_tot <- as.numeric(difftime(time1 = fin_tot, time2 = debut_tot, units = "secs"))
  simul_max <- max(list_temps)
  simul_moy <- mean(list_temps)
  cat('\nNombre de simulation :',nr*repetition,'\n')
  cat("Temps moyen par simulation :",.describe_time(simul_moy),'\n')
  cat("Temps de la simulation la plus longue :",.describe_time(simul_max),'\n')
  cat("Temps total :",.describe_time(temps_tot),'\n\n')
  return(cbind(data_init,data_res))
}




## SIMULATION EN SERIE PARALLELISEE :
parallel.simulation <- function(parametres.fixes,
                                parametres.dens_dep,
                                nb_couple_init = 50,
                                quantite_introduite = 10,
                                nombre_introduction = 1, 
                                male_ratio = 0,
                                mated_ratio = 1,
                                premiere_introduction = 100, 
                                interval_intro = 14,
                                synchro = character(),
                                pourcentage = T,
                                repetition = 1,
                                DD_by_types = T,
                                ref = "Ninf",
                                temps_ref = 1000){
  debut_tot <- Sys.time()
  cat('Working ...\nCould take a while')
  var_names <- c("set_parametre","set_densite_dep",
                 "nb_couple_init","quantite_introduite",
                 "nombre_introduction","male_ratio","mated_ratio",
                 "premiere_introduction","interval_intro")
  if(any(! synchro %in% var_names[-c(1,2)])){
    stop("synchro comprend des noms de variables frauduleux")
  }
  nb_lignes <- sapply(synchro, function(name) length(eval(parse(text=name))))
  if(length(nb_lignes) != 0 && any(nb_lignes != nb_lignes[[1]])){
    stop("synchro contain variable names of different lengths")
  }
  parametres_numeric <- expand.grid(nb_couple_init = nb_couple_init,
                                    quantite_introduite = quantite_introduite,
                                    nombre_introduction = nombre_introduction, 
                                    male_ratio = male_ratio,
                                    mated_ratio = mated_ratio,
                                    premiere_introduction = premiere_introduction, 
                                    interval_intro = interval_intro)
  if(length(nb_lignes) != 0){
    lines <-  T
    for(i in synchro){
      lines <- lines & parametres_numeric[[i]] == eval(parse(text=i))
    }
    parametres_numeric <- parametres_numeric[which(lines),]
  }
  parametres_numeric <- parametres_numeric[rep(seq_len(dim(parametres_numeric)[[1]]), 
                                               each = dim(parametres.fixes)[[1]]*length(parametres.dens_dep)),]
  nr <- dim(parametres_numeric)[1]
  
  ## Liste des arguments
  arg <- list(parametres.fixes,
              parametres.dens_dep,
              parametres_numeric$nb_couple_init,
              parametres_numeric$quantite_introduite,
              parametres_numeric$nombre_introduction, 
              parametres_numeric$male_ratio,
              parametres_numeric$mated_ratio,
              parametres_numeric$premiere_introduction, 
              parametres_numeric$interval_intro)
  names(arg) <- var_names
  nb_col <- length(var_names)
  ## Transforme en listes les agruments non numeriques
  ### Parametres fixes
  id_PF <- rownames(arg[[1]])
  arg[[1]] <- split(arg[[1]], seq(nrow(arg[[1]])))
  
  ### Parametres densite_dep
  if(class(arg[[2]])[[1]]!='list'){
    arg[[2]] <- list(arg[[2]])
  }
  ## Nommes les set de densite dep 
  if(!is.null(names(arg[[2]]))){
    id_DD <- names(arg[[2]])
  }else if(DD_by_types){
    id_DD <- c()
    for(i in arg[[2]]){
      id_DD <- c(id_DD, paste(i$type, collapse = '_'))
    }
  }else{
    id_DD <- seq_len(length(arg[[2]]))
  }
  arg[[1]] <- arg[[1]][rep(rep(seq_len(length(arg[[1]])),length(id_DD)),
                           nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep)))]
  arg[[2]] <- arg[[2]][rep(rep(seq_len(length(arg[[2]])),each=length(id_PF)),
                           nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep)))]
  
  ## Donnee d'entree de simulation
  data_init <- data.frame(matrix(NA,nr*repetition,nb_col+1))
  names(data_init) <- c("id_cas",var_names)
  for (i in seq_len(nb_col)){
    ## Remplie le tableau d'entree
    if(i==1){
      data_init[[i+1]] <- rep(rep(rep(id_PF,length(id_DD)),
                                  nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep))),each=repetition)
    }else if(i==2){
      data_init[[i+1]] <- rep(rep(rep(id_DD,each = length(id_PF)),
                                  nr/(dim(parametres.fixes)[[1]]*length(parametres.dens_dep))),each=repetition)
    }else{
      data_init[[i+1]] <- unlist(arg[[i]][rep(seq_len(nr),each=repetition)])
    }
  }
  data_init$id_cas <- rep(seq_len(nr), each = repetition)
  
  ### Creation du cluster local
  Ncpus <- detectCores() - 1
  cl <- makeCluster(Ncpus)
  envir_util <- c(ls(envir=globalenv(),all.names=T),"rollmean")
  clusterExport(cl,envir_util)
  registerDoParallel(cl)
  par_res <- foreach (i = rep(seq_len(nr),each=repetition), j = rep(seq_len(repetition),nr) ,.combine='rbind') %dopar% {
    is.ref <- data_init$set_parametre[[(i-1)*repetition+j]] %in% ref
    if(is.ref){
      temps_max <- temps_ref
    }else{
      temps_max <- NULL
    }
    debut_simul <- Sys.time()
    pop <- simulation(parametres.fixes = arg$set_parametre[[i]],
                      parametres.dens_dep = arg$set_densite_dep[[i]],
                      nb_couple_init = arg$nb_couple_init[[i]],
                      quantite_introduite = arg$quantite_introduite[[i]],
                      nombre_introduction = arg$nombre_introduction[[i]], 
                      male_ratio = arg$male_ratio[[i]],
                      mated_ratio = arg$mated_ratio[[i]],
                      premiere_introduction = arg$premiere_introduction[[i]], 
                      interval_intro = arg$interval_intro[[i]],
                      pourcentage = pourcentage,
                      summarise_vec = F,
                      temps_max = temps_max)
    fin_simul <- Sys.time()
    time <- as.numeric(difftime(time1 = fin_simul, time2 = debut_simul, units = "secs"))
    ## Remplie le tableau de sortie
    decription <- pop$description(forced = is.ref)
    return(cbind(decription,time=time))
  }
  ### Fermeture du Cluster
  stopCluster(cl)
  
  # Extraction des resulats 
  list_temps <- par_res$time
  data_res <- par_res[1:3]
  fin_tot <- Sys.time()
  temps_tot <- as.numeric(difftime(time1 = fin_tot, time2 = debut_tot, units = "secs"))
  simul_max <- max(list_temps)
  simul_moy <- mean(list_temps)
  gain <- (1-temps_tot/(nr*repetition*simul_moy))*100
  cat('\nNombre de simulation :',nr*repetition,'\n')
  cat("Temps moyen par simulation :",.describe_time(simul_moy),'\n')
  cat("Temps de la simulation la plus longue :",.describe_time(simul_max),'\n')
  cat("Temps total :",.describe_time(temps_tot),'\n')
  cat("Gain parallelisation :",round(gain,1),'%\n\n')
  return(cbind(data_init,data_res))
}



if(stochastique){
  formals(multi.simulation)$repetition <- 15
  formals(parallel.simulation)$repetition <- 15
}
cat("multi.simulation & parallel.simulation -> Nombre de repetition par cas par defaut :",formals(multi.simulation)$repetition,'\n\n')

rm(list=c('InstalledPackage','available','libraries','library','stochastique'))

