
#######################
### DEBUT FONCTIONS ###
#######################

# Ajust peut prendre differents agruments, vecteurs, liste de vecteur ou dataframe
# elle renvoie une liste contenant les differents vecteur ou une liste de meme format 
# que la liste/data.frame en ayant force la taille des vecteur
ajust <- function(obj=NULL,vec2=NULL,..., nb_vec = 2, length = 1, forced = FALSE){
  UseMethod(".ajust",obj)
}

# Fabrique une liste de vecteurs de meme taille a partir de vecteur
.ajust.default <- function(vec1,vec2,..., nb_vec = 2, length = 1, forced = FALSE){
  # Si l'appel est de la forme ajust(,c(1,2)), ajust(c(1,2),) ou ajust(c(1,2))
  # alors vec1 et vec2 prennent par defaut la valeur NULL
  if(missing(vec1)){
    vec1 <- NULL
  }
  if(missing(vec2)){
    vec2 <- NULL
  }
  # Liste des vecteurs en arguments
  arg <- list(vec1,vec2,...)
  # Si aucun agrument n'est specifie en plus et que vec1 et vec2 sont par 
  # defaut null fabrication du resulat par defaut
  if (length(arg) == 2 & is.null(vec1) & is.null(vec2)){
    # length et nb_vec bon format ?
    if (!is.numeric(length) || length(length) != 1 || as.integer(length) != length || length <= 0){
      stop("length dois etre un entier positif")
    }else if (!is.numeric(nb_vec) || length(nb_vec) != 1 || as.integer(nb_vec) != nb_vec || nb_vec <= 0){
      stop("nb_vec dois etre un entier positif")
    }else{
      # premier vecteur de longueur length
      res <- list(rep(0,length))
      # on le renvoie seul si nb_vec=1
      if (nb_vec == 1){
        return(res)
      }
      # on rejoute les nb_vec-1 veteurs manquant
      for (i in 2:nb_vec){
        res <- c(res,list(rep(0,length)))
      }
      return(res)
    }
  }else{
    # Ici c'est le cas ou au moins 1 vecteur est non null alors arg 
    # les contients tous (au moins deux vecteur, même l'un d'eux peut etre vide)
    # longueur maximale
    len <- max(sapply(arg,length))
    if (len == 0){
      len <- length
    }
    for (i in 1:length(arg)){
      # ajustement des taille des vecteurs manquant
      arg[[i]] <- c(rep(0,len-length(arg[[i]])),arg[[i]])
    }
    if(forced){
      if(length < length(arg[[1]])){
        warning(paste("when use forced option : length must be higher than the one from the longest vector\n  Length has been set to",len))
      }else{
        arg <- lapply(arg,function(x,len.max = length,len.min = len){return(c(rep(0,len.max-len.min),x))})
      }
    }
    return(arg)
  }
}
# Fabrique une liste de vecteurs de meme taille a partir d'une data.frame
.ajust.data.frame <- function(data,vec2,..., nb_vec = 2, length = 1, forced = FALSE){
  # change dataf in list
  data<-as.list(data)
  names(data) <- NULL
  len <- length(data[[1]])
  # si l'on force la taille de la dataf
  if(forced){
    # on ne peut la rendre plus petite
    if(length < length(data[[1]])){
      warning(paste("when use forced option : length must be higher than the one from the longest vector\n  Length has been set to",len))
    }else{
      # on ralonge les vecteurs
      data <- lapply(data,function(x,len.max = length,len.min = len){return(c(rep(0,len.max-len.min),x))})
    }
  }
  return(data)
}
# Fabrique une liste de vecteurs de meme taille a partir d'une list
.ajust.list <- function(list_of_vector,vec2,..., nb_vec = 2, length = 1, forced = FALSE){
  len <- max(sapply(list_of_vector,length))
  if (len == 0){
    len <- length
  }
  for (i in 1:length(list_of_vector)){
    # ajustement des taille des vecteurs manquant
    list_of_vector[[i]] <- c(rep(0,len-length(list_of_vector[[i]])),list_of_vector[[i]])
  }
  if(forced){
    if(length < length(list_of_vector[[1]])){
      warning(paste("when use forced option : length must be higher than the one from the longest vector\n  Length has been set to",len))
    }else{
      list_of_vector <- lapply(list_of_vector,function(x,len.max = length,len.min = len){return(c(rep(0,len.max-len.min),x))})
    }
  }
  return(list_of_vector)
}

# Fonction completant un vecteur avec des NA's en remplaçant par la valeur non NA a gauche
complete <- function(x){
  if(all(is.na(x))){
    stop("x could not be fulled because there is no other value than NA")
  }
  for (i in 1:length(x)){
    if(is.na(x[[i]])){
      if(i==1){
        x[i] <- x[[min(which(!is.na(x)))]]
      }else{
        x[i] <- x[[i-1]]
      }
    }
  }
  return(x)
}

# Prend une liste d'ages de larve et renvoie la liste des probabilite d'emmergerer a cet age
.proba_larves <- function(age, temps_dev, ampli = 6){
  return(ifelse((age < temps_dev - ampli)|
                  (age > temps_dev + ampli), # pour les ages inferieurs a moy - ampli et superieur a age + ampli
                0, # on renvois 0
                ifelse(age==temps_dev+ampli,
                       1,# remplace des valeur estimee proche de 1 par 1
                       1/(1+exp(-0.53*(age-temps_dev))))))# pour les autres on renvoie une probabilite d'eclosion sigmoidale (forme empirique)
}

#######################
###  FIN FONCTIONS  ###
#######################

#####################################
### DEBUT CLASSES POUR CATEGORIES ###
#####################################


### CLASSE DES OEUFS ###



.eggs<-R6Class(classname = "eggs",
              ## L'ensemble de la classe est contenue dans public ##
              # t est le temps de simulation et ninf est le vecteur contenant les donnes des non infectees et inf est celui des infectees
              public = list(t=NA,
                            ninf=NA,
                            inf=NA,
                            # C'est le generateur de Classe, il s'utilise avec l'appel : eggs$new(...) il prend pour agrument,
                            # deux vecteur ou une liste, ou dataframe de deux colonne
                            initialize=function(list_non_inf=NULL,list_inf=NULL,t=1, forced = FALSE){
                              # dans le cas du data.frame ou de la liste on verifie la taille
                              if(is.list(list_non_inf) & length(list_non_inf) != 2){
                                stop("list or data.frame object must have 2 columns for eggs")
                              }
                              # Si list non_inf est une list ou data.frame, list_infne sera pas integre
                              # sinon les deux vecteurs seront de même longueurs
                              list <- ajust(list_non_inf, list_inf, length = t, forced = forced)
                              self$ninf <- list[[1]]
                              self$inf <- list[[2]]
                              self$t <- length(list[[1]])
                            },
                            # Genere un data-frame pour la categorie eggs
                            data_show=function(){
                              # Ici il contiendra seulement une liste des jours et les oeufs pondus a chaque date
                              show <- data.frame(Jours=1:self$t,Egg_Non_Infectes=self$ninf,Egg_Infectes=self$inf)
                              return(show)
                            },
                            # Permet l'affichage propre basee sur la methode data_show()
                            # Elle se compilera sur la methode print generique.
                            print=function(){
                              cat("\nTemps = ",self$t," jours\n\n")
                              print(self$data_show())
                            },
                            # Permet de voir si le format est coerrant, elle detecte les erreurs
                            check=function(){
                              # les valeurs doivent toujours etre de longueur t
                              t_check=length(self$ninf)==self$t & length(self$inf)==self$t
                              # les valeurs doivent etre numeric
                              v_check=is.numeric(self$ninf) & is.numeric(self$inf)
                              # les valeurs doivent etres positives
                              pos_check=all(self$ninf>=0) & all(self$inf>=0)
                              return(t_check & v_check & pos_check)
                            },
                            # Permet l'ajout d'une nouvelle valeur aux vecteurs et de 1 a t
                            # new_values est un vecteur de 2 elements
                            next_val=function(new_values){
                              self$t <- self$t+1
                              self$ninf <- c(self$ninf,new_values[1])
                              self$inf <- c(self$inf,new_values[2])
                              invisible(self)
                            },
                            # Renvoie le nombre d'oeufs qui eclosent
                            survie=function(temps_eclo,survie_egg){
                              if(self$t<temps_eclo){
                                # Aucun oeufs n'eclo si le temps d'eclosion n'est pas passe
                                return(c(0,0))
                              }
                              # Definie les oeufs allant potentiellement eclore
                              nb_egg <- c(self$ninf[self$t+1-temps_eclo],self$inf[self$t+1-temps_eclo])
                              # Simule une loi Binomiale de type B(nboeufs, proba_survie)
                              return(nb_egg*survie_egg)
                            }
              ))

### CLASSE DES LARVES ###

.larves<-R6Class(classname = "larves",
                 ## Voir egg si probleme, larve possede un format similaire
                 # Seules les differences seront expliquee
                 #Idem pour t, ninf et inf
                 public = list(t=NA,
                               ninf=NA,
                               inf=NA,
                               initialize=function(list_non_inf=NULL,list_inf=NULL,t=1,forced = FALSE){
                                 if(is.list(list_non_inf) & length(list_non_inf) != 2){
                                   stop("list or data.frame objecthave 2 columns for larves")
                                 }
                                 list <- ajust(list_non_inf, list_inf, length = t, forced = forced)
                                 self$ninf <- list[[1]]
                                 self$inf <- list[[2]]
                                 self$t <- length(list[[1]])
                               },
                               # Permet de compter le nombre de larves encore vivant
                               vivants=function(){
                                 # C'est simplement la somme des ninf et inf
                                 return(c(sum(self$ninf),sum(self$inf)))
                               },
                               # Idem
                               data_show=function(){
                                 show<-data.frame(Jours=1:self$t,Larve_Non_Infectes=self$ninf,Larve_Infectes=self$inf)
                                 return(show)
                               },
                               # Idem
                               print=function(){
                                 cat("\nTemps = ",self$t," jours\n\n")
                                 print(self$data_show())
                               },
                               # Idem
                               check=function(){
                                 t_check=length(self$ninf)==self$t & length(self$inf)==self$t
                                 v_check=is.numeric(self$ninf) & is.numeric(self$inf)
                                 pos_check=all(self$ninf>=0) & all(self$inf>=0)
                                 return(t_check & v_check & pos_check)
                               },
                               # Idem
                               next_val=function(new_values){
                                 self$t <- self$t+1
                                 self$ninf <- c(self$ninf,new_values[1])
                                 self$inf <- c(self$inf,new_values[2])
                                 invisible(self)
                               },
                               # Survie sera differente, en effet les larves peuvent donner a la fois des vierges et des males
                               # Contrairement aux oeufs qui donnent seulement des larves
                               survie=function(temps_dev, survie_larves, taux_femelle = 0.5){
                                 # probabilite de survie celon les ages
                                 proba <- .proba_larves(rev(1:self$t),temps_dev)
                                 # liste des emmegents non infecte
                                 emergents_ninf <- self$ninf*proba
                                 # liste des emmegents infecte
                                 emergents_inf <- self$inf*proba
                                 # retrait des emmegents ninf
                                 self$ninf <- self$ninf - emergents_ninf
                                 # retrait des emmegents infecte
                                 self$inf <- self$inf - emergents_inf
                                 # on compte les emergents
                                 nb_larves <- c(sum(emergents_ninf),sum(emergents_inf))
                                 # Tirage aleatoire pour la survie des larves ayant emmergees
                                 nb_adulte <- nb_larves*survie_larves
                                 # Tirage aleatoire pour la determination du sexe
                                 nb_fem <- nb_adulte*taux_femelle
                                 # Les autres survivants sont des males
                                 res <- data.frame(vierges = nb_fem, males = nb_adulte - nb_fem)
                                 return(res)
                               }
                 ))

### CLASSE DES MALES ###

.males<-R6Class(classname = "males",
               ## Voir egg si probleme, male possede un format similaire
               # Seules les differences seront expliquee
               public = list(t=NA,
                             ninf=NA,
                             inf=NA,
                             initialize=function(list_non_inf=NULL,list_inf=NULL,t=1,forced = FALSE){
                               if(is.list(list_non_inf) & length(list_non_inf) != 2){
                                 stop("list or data.frame object must have 2 columns for males")
                               }
                               list <- ajust(list_non_inf, list_inf, length = t, forced = forced)
                               self$ninf <- list[[1]]
                               self$inf <- list[[2]]
                               self$t <- length(list[[1]])
                             },
                             # Pour male, vierge et femelle, la derniere ligne des
                             # vecteurs contient le nombre d'individu vivants a t.
                             # Contrairement aux larves et aux oeufs pas de duree de vie fixe donc pas de parametre
                             vivants=function(t = self$t){
                               # On appelle donc uniquement la ligne t
                               return(c(self$ninf[t],self$inf[t]))
                             },
                             # Idem
                             data_show=function(){
                               show<-data.frame(Jours=1:self$t,Male_Non_Infectes=self$ninf,Male_Infectes=self$inf)
                               return(show)
                             },
                             # Idem
                             print=function(){
                               cat("\nTemps = ",self$t," jours\n\n")
                               print(self$data_show())
                             },
                             # Idem
                             check=function(){
                               t_check=length(self$ninf)==self$t & length(self$inf)==self$t
                               v_check=is.numeric(self$ninf) & is.numeric(self$inf)
                               pos_check=all(self$ninf>=0) & all(self$inf>=0)
                               return(t_check & v_check & pos_check)
                             },
                             # Idem
                             next_val=function(new_values){
                               self$t <- self$t+1
                               self$ninf <- c(self$ninf,new_values[1])
                               self$inf <- c(self$inf,new_values[2])
                               invisible(self)
                             },
                             # Ici survie vas changer :
                             # Les males possedent un taux de survie quotidien on tire donc alleatoirement les males qui survivent
                             survie=function(taux_survie){
                               return(self$vivants()*taux_survie)
                             },
                             # Calcule la proportion de males sains prmis les males vivants
                             proportion=function(t=self$t){
                               nb_males <- self$vivants(t)
                               if (sum(nb_males) == 0){
                                 return(0)
                               }
                               return(nb_males[1]/sum(nb_males))
                             }
               ))


### CLASSE DES VIERGES ###

.vierges<-R6Class(classname = "vierges",
                 ## Voir males si probleme, vierge possede un format similaire
                 # Seules les differences seront expliquee
                 # Idem pour t et les vecteurs
                 public = list(t=NA,
                               ninf=NA,
                               inf=NA,
                               initialize=function(list_non_inf=NULL,list_inf=NULL,t=1,forced = FALSE){
                                 if(is.list(list_non_inf) & length(list_non_inf) != 2){
                                   stop("list or data.frame object must have 2 columns for vierges")
                                 }
                                 list <- ajust(list_non_inf, list_inf, length = t, forced = forced)
                                 self$ninf <- list[[1]]
                                 self$inf <- list[[2]]
                                 self$t <- length(list[[1]])
                               },
                               # Idem
                               vivants=function(t = self$t){
                                 return(c(self$ninf[t],self$inf[t]))
                               },
                               # Idem
                               data_show=function(){
                                 show<-data.frame(Jours=1:self$t,Vierge_Non_Infectes=self$ninf,Vierge_Infectes=self$inf)
                                 return(show)
                               },
                               # Idem
                               print=function(){
                                 cat("\nTemps = ",self$t," jours\n\n")
                                 print(self$data_show())
                               },
                               # Idem
                               check=function(){
                                 t_check=length(self$ninf)==self$t & length(self$inf)==self$t
                                 v_check=is.numeric(self$ninf) & is.numeric(self$inf)
                                 pos_check=all(self$ninf>=0) & all(self$inf>=0)
                                 return(t_check & v_check & pos_check)
                               },
                               # Idem
                               next_val=function(new_values){
                                 self$t <- self$t+1
                                 self$ninf <- c(self$ninf,new_values[1])
                                 self$inf <- c(self$inf,new_values[2])
                                 invisible(self)
                               },
                               # La survie des vierges est modifiee car contrairement 
                               # aux males les vierges peuvent s'accouplee et changer de categorie
                               survie=function(taux_survie, proba_accoupl,prop_male_sains){
                                 # D'abord la survie identique aux males 
                                 nb_survi <- self$vivants()*taux_survie
                                 # Puis l'accouplement allant definir les nouvelles femelles et leur categorie
                                 nb_accoupl <- nb_survi*proba_accoupl
                                 # definitions des femelles s'etant accouple avec un male sain
                                 # le vecteur calcule donnera donc : ninf x ninf et inf x ninf
                                 acc.ninf <- nb_accoupl*prop_male_sains
                                 # definitions des femelles s'etant accouple avec un male sain
                                 # le vecteur calcule donnera donc : ninf x inf et inf x inf
                                 acc.inf <- nb_accoupl - acc.ninf
                                 # Celle ne s'étant pas accouplee resteronts des vierges
                                 res <- list(vierges = nb_survi - nb_accoupl, femelles = c(acc.ninf[1],acc.inf[1],acc.ninf[2],acc.inf[2]))
                                 return(res)
                               }
                 ))

### CLASSE DES FEMELLES ###

.femelles<-R6Class(classname = "femelles",
                  ## les femmelles auront 4 categories selon leur accouplement
                  public = list(t=NA,
                                # ninf.inf sont les femelles non-infecte accouplees avec un mal infecte etc...
                                ninf.ninf=NA,
                                ninf.inf=NA,
                                inf.ninf=NA,
                                inf.inf=NA,
                                # Idem a eggs pas de femelle fecondee a t=1
                                initialize=function(list_ninf.ninf=NULL,list_ninf.inf=NULL,list_inf.ninf=NULL,list_inf.inf=NULL,t=1,forced = FALSE){
                                  if(is.list(list_ninf.ninf) & length(list_ninf.ninf) != 4){
                                    stop("list or data.frame object must have 4 columns for femelles")
                                  }
                                  list <- ajust(list_ninf.ninf,list_ninf.inf,list_inf.ninf,list_inf.inf, length = t, forced = forced)
                                  self$ninf.ninf <- list[[1]]
                                  self$ninf.inf <- list[[2]]
                                  self$inf.ninf <- list[[3]]
                                  self$inf.inf <- list[[4]]
                                  self$t <- length(list[[1]])
                                },
                                # Idem aux males
                                vivants=function(t=self$t){
                                  return(c(self$ninf.ninf[t],self$ninf.inf[t],self$inf.ninf[t],self$inf.inf[t]))
                                },
                                # Idem
                                data_show=function(accouplements = FALSE){
                                  # on separe les categories par accouplement 
                                  if (accouplements){
                                    show<-data.frame(Jours=1:self$t,Femelles_NInf.NInf=self$ninf.ninf,
                                                     Femelles_NInf.Inf=self$ninf.inf,Femelles_Inf.NInf=self$inf.ninf,
                                                     Femelles_Inf.Inf=self$inf.inf)
                                  }else{
                                    show<-data.frame(Jours=1:self$t,Femelles_Non_Infectes=self$ninf.ninf+self$ninf.inf,
                                                     Femelles_Infectes=self$inf.ninf+self$inf.inf)
                                  }
                                  return(show)
                                },
                                # Idem
                                print=function(accouplements = FALSE){
                                  cat("\nTemps = ",self$t," jours\n\n")
                                  print(self$data_show(accouplements))
                                },
                                # Idem
                                check=function(){
                                  t_check=length(self$ninf.ninf)==self$t & length(self$ninf.inf)==self$t & length(self$inf.ninf)==self$t & length(self$inf.inf)==self$t
                                  v_check=is.numeric(self$ninf.ninf) & is.numeric(self$ninf.inf) & is.numeric(self$inf.ninf) & is.numeric(self$inf.inf)
                                  pos_check=all(self$ninf.ninf>=0) & all(self$ninf.inf>=0) & all(self$inf.ninf>=0) & all(self$inf.inf>=0)
                                  return(t_check & v_check & pos_check)
                                },
                                # Idem
                                next_val=function(new_values){
                                  self$t <- self$t+1
                                  self$ninf.ninf <- c(self$ninf.ninf,new_values[1])
                                  self$ninf.inf <- c(self$ninf.inf,new_values[2])
                                  self$inf.ninf <- c(self$inf.ninf,new_values[3])
                                  self$inf.inf <- c(self$inf.inf,new_values[4])
                                  invisible(self)
                                },
                                # Idem aux males
                                survie=function(taux_survie){
                                  return(self$vivants()*taux_survie)
                                },
                                # Simule une loi de poisson de type P(fecondites x acces ponte x proba_viable )
                                # et renvoie le nombre d'oeufs produits
                                # Ic doit forcements etre de la forme;
                                # meme organisation pour les vecteur de cette fonction : c(--,-+,+-,++)
                                ponte=function(fec_ninf,fec_inf,proba_ponte,transmissibilite,ic){
                                  # proportion de transmission de wolbacchia
                                  transmissibilite <- c(0,0,transmissibilite,transmissibilite)
                                  # nombre d'oeufs moyen quotidien
                                  fecondite <- c(rep(fec_ninf,2),rep(fec_inf,2))
                                  # proportion d'oeufs viable
                                  proba_viable <- c(1,1-ic,1,1)
                                  # tirage des mouches pouvant pondre
                                  nb_acces_ponte <- self$vivants()*proba_ponte
                                  nb_oeufs <- rep(NA,2)
                                  # tirage du nombre d'oeufs viables par categorie
                                  nb_oeufs[1] <- sum(fecondite*nb_acces_ponte*proba_viable*(1-transmissibilite))
                                  nb_oeufs[2] <- sum(fecondite*nb_acces_ponte*proba_viable*transmissibilite)
                                  return(nb_oeufs)
                                }
                  ))


#####################################
###  FIN CLASSES POUR CATEGORIES  ###
#####################################

####################################
### DEBUT CLASSE POUR POPULATION ###
####################################


# pour tout les objets de type R6 permet l'ecriture obj[indice] au lieu de obj$`[`(indice)
`[.R6` <- function(x, ...) x$`[`(...) 

population<-R6Class(classname = 'population',
                    # t est le temps de la simulation
                    # egg, larve, male, femelle et vierge sont des element des categories presentee precedement
                    public = list(t=NA,
                                  egg=NA,
                                  larve=NA,
                                  male=NA,
                                  vierge=NA,
                                  femelle=NA,
                                  parametres.fixes=NA,
                                  parametres.fonctions=NA,
                                  # Fonction generatrice de la classe,  les argument permettent d'entrer une population en cours
                                  initialize=function(parametres,fonctions,egg=NULL,larve=NULL,male=NULL,vierge=NULL,femelle=NULL,t=1){
                                    egg <- ajust(egg,NULL,length=t)
                                    larve <- ajust(larve,NULL,length=t)
                                    if(is.null(male)){
                                      male <- ajust(50,NULL,length=t)
                                    }else{
                                      male <- ajust(male,NULL,length=t)
                                    }
                                    if(is.null(vierge)){
                                      vierge <- ajust(50,NULL,length=t)
                                    }else{
                                      vierge <- ajust(vierge,NULL,length=t)
                                    }
                                    femelle <- ajust(femelle,NULL,NULL,NULL,length=t)
                                    list_cat <- list(egg,larve,male,vierge,femelle)
                                    self$t <- max(c(sapply(list_cat,function(x){return(length(x[[1]]))}),t))
                                    # Defenition de chaque categorie soit par celle en agrument, 
                                    # soit si elle n'existe pas par celle de refference de longueur t
                                    self$egg<-.eggs$new(egg, t = self$t, forced=T)
                                    self$larve<-.larves$new(larve, t = self$t, forced=T)
                                    self$male<-.males$new(male, t = self$t, forced=T)
                                    self$vierge<-.vierges$new(vierge, t = self$t, forced=T)
                                    self$femelle<-.femelles$new(femelle, t = self$t, forced=T)
                                    self$parametres.fixes <- parametres
                                    self$parametres.fonctions <- fonctions
                                    # Teste ensuite si l'objet a le bon format
                                    if(!self$check()){
                                      return(self)
                                    }
                                  },
                                  # Affiche les juveniles (larves et oeufs) a un temps t presisé ou au dernier t sinon,
                                  # peut aussi les afficher pour chaque t en une unique collone si all=TRUE
                                  juveniles=function(t=self$t,all=F){
                                    if(!all){
                                      # Si all est FALSE
                                      # On affiche on somme les larves et oeufs par infection
                                    return(c(self$egg$ninf[t]+self$larve$ninf[t],self$egg$inf[t]+self$larve$inf[t]))
                                    }
                                    data_egg <- self$egg$data_show() 
                                    data_larve <- self$larve$data_show() 
                                    data_juv <- data.frame(Jours = data_egg[[1]], Juveniles_NInf = data_egg[[2]]+data_larve[[2]],
                                                           Juveniles_Inf= data_egg[[3]]+data_larve[[3]])
                                    # Sinon on renvoie les juveniles a chaque temps
                                    return(data_juv)
                                  },
                                  # Fonctionne comme juvenilles mais pour les adultes, 
                                  # elle fais la somme de male, vierge et femelle
                                  adultes=function(t=self$t,vector=F,all=F){
                                    if(vector){
                                      data=self$data_show()
                                      return(data$Adultes_NInf+data$Adultes_Inf)
                                    }
                                    if(!all){
                                      return(c(self$male$ninf[t]+self$vierge$ninf[t]+self$femelle$ninf.ninf[t]+self$femelle$ninf.inf[t],
                                               self$male$inf[t]+self$vierge$inf[t]+self$femelle$inf.ninf[t]+self$femelle$inf.inf[t]))
                                    }
                                    data_male <- self$male$data_show() 
                                    data_vierge <- self$vierge$data_show()
                                    data_femelle <- self$femelle$data_show() 
                                    data_adu <- data.frame(Jours = data_male[[1]], Adultes_NInf = data_male[[2]]+data_vierge[[2]]+data_femelle[[2]],
                                                           Adultes_Inf= data_male[[3]]+data_vierge[[3]]+data_femelle[[3]])
                                    return(data_adu)
                                  },
                                  # Genere un data-frame 
                                  data_show=function(resume=T,accouplements=F){
                                    if(resume){
                                      # affiche uniquement les juveniles et adultes
                                      return(data.frame(self$juveniles(all=T),self$adultes(all=T)[-1]))
                                    }else{
                                      # affiche separement les categories, accouplement == T decompose les femmelles par accouplement
                                      return(data.frame(self$egg$data_show(),self$larve$data_show()[-1],
                                                        self$male$data_show()[-1],self$vierge$data_show()[-1],
                                                        self$femelle$data_show(accouplements)[-1]))
                                    }
                                  },
                                  # Verifie le format de la population, renvoie TRUE si correcte, 
                                  # FALSE sinon et print un message d'erreur adapte
                                  check=function(){
                                    # Fait un check() pour chaque categorie
                                    each_check <- self$egg$check() & self$larve$check() & self$male$check() & self$vierge$check() & self$femelle$check()
                                    if(!each_check){
                                      stop("Erreur dans une catégorie")
                                    }
                                    # Verifie l'egalite de tout les temps et donc de tous les formats
                                    check_time <- self$egg$t==self$t & self$larve$t==self$t & self$male$t==self$t & self$vierge$t==self$t & self$femelle$t==self$t
                                    if(!check_time){
                                      stop("Erreur dans le codage de la population")
                                    }
                                    return(each_check & check_time)
                                  },
                                  # Fait un print utilisant le data_show()
                                  # Si short = F affiche tout le tableau, affiche les 6 dernière lignes sinon
                                  print=function(short = F){
                                    # Verifie le format prealablement
                                    if(self$check()){
                                      # Si le tableau fait plus de 6 lignes et que short = T 
                                      if(short & self$t>6){
                                        cat("\nTemps = ",self$t," jours\n\n")
                                        # Alors on affiche uniquement les 6 dernières lignes du tableau
                                        print(self$data_show()[(self$t-6):self$t,])
                                      }else{
                                        cat("\nTemps = ",self$t," jours\n\n")
                                        # On affiche tout sinon
                                        print(self$data_show())
                                      }
                                    }
                                  },
                                  # Fais le pas de temps pour la population
                                  # Param est un set de parametre dont le format doit etre celui de "parametres.csv", incluant le nom des collonnes :
                                  # Pour plus d'information voir le fichier ou decendre dans la section parametre.
                                  step=function(vec_invasion = list(0,0,0), pourcentage = TRUE,param=self$parametres.fixes,fonct=self$parametres.fonctions){
                                    # Les nouvelles larves sont definie par la methode survie de egg
                                    new_larve <- self$egg$survie(param$temps_eclo, param$survie_egg)
                                    # Les nouveaux adultes seront les vierges et les males produits par survie larve
                                    new_adultes <- self$larve$survie(param$temps_dev, fonct$survie_larve(sum(self$larve$vivants())), param$taux_femelle)
                                    # La nouvelle organisation des anciennes vierges est obtenues par survie vierge
                                    new_orga_vierge <- self$vierge$survie(fonct$survie_adulte(sum(self$adultes())), fonct$proba_accoupl(sum(self$male$vivants())*sum(self$male$vivants())),self$male$proportion())
                                    # La ponte des oeufs par femelle definie les nouveaux oeufs
                                    new_egg <- self$femelle$ponte(param$fecondite_ninf,param$fecondite_inf,fonct$proba_ponte(sum(self$adultes())),param$transmissibilite,param$IC)
                                    # Les prochains males sont formes par les survivants et les nouveaux venus
                                    new_male <- self$male$survie(fonct$survie_adulte(sum(self$adultes()))) + new_adultes$males
                                    # addition des males infectes
                                    if (self$t <= length(vec_invasion[[1]])){
                                      new_male[2] <- new_male[2] + ifelse(pourcentage,round(vec_invasion[[1]][self$t]*sum(self$adultes())/100),vec_invasion[[1]][self$t])
                                    }
                                    # Les prochaines vierges sont formes par les survivantes non accouplees et les nouvelles venues
                                    new_vierge <- new_orga_vierge$vierges + new_adultes$vierges
                                    if (self$t <= length(vec_invasion[[3]])){
                                      new_vierge[2] <- new_vierge[2] + ifelse(pourcentage,round(vec_invasion[[3]][self$t]*sum(self$adultes())/100),vec_invasion[[3]][self$t])
                                    }
                                    # Les prochaines femelles sont formes par les survivantes et les vierges accouplees
                                    new_femelle <- self$femelle$survie(fonct$survie_adulte(sum(self$adultes()))) + new_orga_vierge$femelles
                                    # addition des vierges infectes
                                    if (self$t <= length(vec_invasion[[2]])){
                                      new_femelle[4] <- new_femelle[4] + ifelse(pourcentage,round(vec_invasion[[2]][self$t]*sum(self$adultes())/100),vec_invasion[[2]][self$t])
                                    }
                                    # On sauve la nouvelle population et on ajoute +1 au temps.
                                    self$egg <- self$egg$next_val(new_egg)
                                    self$larve <- self$larve$next_val(new_larve)
                                    self$male <- self$male$next_val(new_male)
                                    self$vierge <- self$vierge$next_val(new_vierge)
                                    self$femelle <- self$femelle$next_val(new_femelle)
                                    self$t<-self$t+1
                                    invisible(self)
                                  },
                                  # Affiche un resume de la population au temps t demande (par defaut le dernier temps)
                                  summary=function(t=self$t){
                                    # verfication au prealable
                                    if(self$check()){
                                      # calcul des juveniles et adultes du temps t
                                      juveniles = self$juveniles(t)
                                      adultes = self$adultes(t)
                                      cat("\nTemps = ",t," jours\n\n")
                                      # Affiche le nombre de juveniles
                                      cat("Nombre de juveniles = ",sum(juveniles))
                                      # Si il y a des juveniles affiche leur composition 
                                      if (sum(juveniles)!=0){
                                        cat("\nParmis eux : ",round(juveniles[[2]]/sum(juveniles)*100,2),"% sont infectes")
                                      }
                                      # Affiche le nombre d'e juveniles d'adultes
                                      cat("\n\nNombre d'adultes = ",sum(adultes),"\n")
                                      if(sum(adultes)!=0){
                                        # Si il y a des adultes affiche leur composition  
                                        cat("Parmis eux : ",round((adultes[[2]])/sum(adultes)*100,2),"% sont infectes\n")
                                      }
                                      cat("\n\n")
                                    }
                                  },
                                  # fonction qui affiche les graphiques
                                  plot=function(pourcentage=FALSE,description=F,type="l",ech_log=T,main=NULL,
                                                sub=NULL,xlab="Jours",ylab=NULL,asp=NA,xlim=NULL,ylim=NULL,legend=TRUE){
                                    if(pourcentage){
                                      # affiche la proportion d'individus infectes
                                      if(is.null(ylab)){
                                        ylab <- "Pourcentage d'infection"
                                      }
                                      if(is.null(ylim)){
                                        ylim <- c(0,1)
                                      }
                                      plot(self$proportion(all = TRUE),type=type,main=main,sub=sub,xlab=xlab,ylab=ylab,asp=asp,xlim=xlim,ylim=ylim)
                                    }else if(!description){
                                      # affichage de la pop totale et des infectes et non infectes
                                      # Generation du tableau complet
                                      data=self$data_show()
                                      sum_adulte <- data$Adultes_NInf+data$Adultes_Inf
                                      if(is.null(ylab)){
                                        ylab <- ifelse(ech_log,"log(Nombre d'adultes)","Nombre d'adultes")
                                      }
                                      if(ech_log){
                                        data <- log(data+1)
                                        sum_adulte <- log(sum_adulte+1)
                                      }
                                      if(is.null(ylim)){
                                        ylim <- c(0,max(sum_adulte)+1)
                                      }
                                      plot(sum_adulte,type=type,main=main,sub=sub,xlab=xlab,ylab=ylab,asp=asp,xlim=xlim,ylim=ylim,col='black')
                                      points(data$Adultes_NInf,type=type,main=main,sub=sub,xlab=xlab,ylab=ylab,asp=asp,xlim=xlim,ylim=ylim,col='blue')
                                      points(data$Adultes_Inf,type=type,main=main,sub=sub,xlab=xlab,ylab=ylab,asp=asp,xlim=xlim,ylim=ylim,col='red')
                                      points(sum_adulte,type=type,main=main,sub=sub,xlab=xlab,ylab=ylab,asp=asp,xlim=xlim,ylim=ylim,col='black')
                                      if(legend){
                                        if(type=="p"){
                                          legend("bottomright",inset=0.05, legend=c("Total","Non infectes", "Infectes"),pch=c(1,1),col=c("black","blue", "red"))
                                        }else{
                                          legend("bottomright",inset=0.05, legend=c("Total","Non infectes", "Infectes"),lty=c(1,1),col=c("black","blue", "red"))
                                        }
                                      }
                                    }else{
                                      adultes <- self$adultes(vector=T)
                                      description <- self$description(all=T)
                                      plot(adultes,type=type,main=main,sub=sub,xlab=xlab,ylab=ylab,asp=asp,xlim=xlim,ylim=ylim,col='black')
                                      lines(x=rep(description$temps_50,2),y=c(0,max(adultes)),col='green',lty=2)
                                      lines(x=c(0,self$t),y=rep(description$asympt,2),col='green',lty=2)
                                    }
                                  },
                                  # rend la proportion d'adultes infectes 
                                  proportion=function(t = self$t, all = FALSE){
                                    if(all){
                                      data=self$data_show()
                                      # le vecteur des proportions si dessous est troue par des NA lorsque le nombre d'adultes est a NA
                                      vec_prop <- data$Adultes_Inf/(data$Adultes_Inf+data$Adultes_NInf)
                                      # complete vas remplacer les NA par la derniere valeur, qui sera 0 ou 1 (utile pour l'affichage de la courbe)
                                      return(complete(vec_prop))
                                    }else{
                                      # renvoie la proportion au temps t
                                      prop <- self$adultes(t)[2]/sum(self$adultes(t))
                                      if(is.na(prop)){
                                        # renvoie un warning pour alerter que le 0 est uniquement par default si il y n'y a pas d'adultes
                                        warning(paste("Pas d'adultes Vivants a t =",t))
                                        return(0)
                                      }
                                      return(prop)
                                    }
                                  },
                                  # permet d'afficher l'interieur de la population en selectionnant category, infections et lignes
                                  `[` = function(category=1:ifelse(detail,4+detail,2),statut_inf=1:2,temps=1:self$t,detail=1){
                                    # detail entre 0,1 et 2 permet le regale du detail dans la pop : 0 -> juv + adu, 1 -> les 5 cate, 2 -> deteail les femelles
                                    # statut_inf influe sur les statuts dinfections selectionnes, 1 non_inf, 2 inf, et 1:2 les deux
                                    # category est le numero de la category (depend du detail choisi), par deffaut, 1 -> egg, 2 -> larve ,..., 5 -> femelles
                                    return(self$data_show(detail==0,detail==2)[temps,rep(category,each=length(statut_inf))*2+statut_inf-1])
                                  },
                                  description = function(all=F,forced=F){
                                    smooth_pop <- rollmean(self$adultes(vector=T),50,na.pad = T)
                                    prop <- self$proportion(all=T)
                                    list0 <- which(prop!=0)
                                    t_intro <- ifelse(length(list0)==0,0,min(list0))
                                    envahie <- self$proportion()>0.5
                                    if(envahie){
                                      pop_max <- max(smooth_pop,na.rm = T)
                                      t_80 <- min(which(smooth_pop>0.8*pop_max))
                                      t_50 <- (max(which(prop<=0.5)) + min(which(prop>=0.5)))/2
                                      asympt <- max(smooth_pop[t_80:self$t],na.rm=T)
                                      t_crible <- max(150,t_intro)
                                      min_pop <- min(smooth_pop[t_crible:self$t],na.rm=T)
                                      dim_pop <- min_pop/asympt
                                    }else if(forced){
                                      pop_max <- max(smooth_pop,na.rm = T)
                                      t_80 <- min(which(smooth_pop>0.8*pop_max))
                                      t_50 <- NA
                                      asympt <- max(smooth_pop[t_80:self$t],na.rm=T)
                                      t_crible <- max(150,t_intro)
                                      min_pop <- min(smooth_pop[t_crible:self$t],na.rm=T)
                                      dim_pop <- min_pop/asympt
                                    }else{
                                      t_50 <- NA
                                      diminution_relative <- NA
                                      asympt <- NA
                                      dim_pop <- NA
                                    }
                                    if(all){
                                      return(list(envahie = envahie, temps_intro = t_intro, temps_50 = t_50,
                                                  diminution_relative = dim_pop, smooth_pop = smooth_pop,
                                                  asympt = asympt))
                                    }
                                    return(data.frame(envahie = envahie, temps_50 = t_50 - t_intro, diminution_relative = dim_pop))
                                  }
                    ))


####################################
###  FIN CLASSE POUR POPULATION  ###
####################################
