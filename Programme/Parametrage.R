###################################
###   DEBUT CODAGE PARAMETRES   ###
###################################

# le fichier parametre contient pour chaque ligne un set de parametres
# au minimum les noms des collones doivent etre :
# names(param) <- c("temps_eclo","survie_egg","temps_dev","taux_femelle",'fecondite_ninf', 'fecondite_inf', 'transmissibilite')

parametres.fixes <-read.csv2("Programme/Parametres.csv",row.names = 1)


# pour tout les objets de type R6 permet l'ecriture obj[indice] au lieu de obj$`[`(indice)
`[.R6` <- function(x, ...) x$`[`(...) 


# defenie, stock et code des fonction, de la meme maniere qu'un dataframe se comporte un peu comme une dataframe
parametres.mobiles <- R6Class(classname = "fonction.list",
                              # ce sont les espaces de stockage des fonctions ils sont nommes comme les parametres
                              public = list(survie_adulte=NA,
                                            survie_larve=NA,
                                            proba_ponte=NA,
                                            proba_accoupl=NA,
                                            type = c(),
                                            # permet de generer un tableau de ce genre, chaque espace de stockage possede 3 pre-set, numerotes de 1 a 3
                                            initialize = function(... , code = NULL, const_larve = NULL, alpha_larve = NULL,
                                                                  const_ponte = NULL, alpha_ponte = NULL, const_adulte = NULL,
                                                                  alpha_adulte = NULL, survie_adulte = NULL, survie_larve = NULL,
                                                                  proba_ponte = NULL, proba_accoupl = NULL){
                                              list_DD <- c('Adultes','Larves','Pontes','Accouplement')
                                              arg_uncalled <- list(...)
                                              if(!is.null(code)){
                                                mode <- 1
                                                new_list <- code
                                              }else if(!(is.null(const_larve) & is.null(const_ponte) & is.null(const_adulte) &
                                                         is.null(alpha_larve) & is.null(alpha_ponte) & is.null(alpha_adulte))){
                                                mode <- 2
                                                defaults <- c(0.67,1,0.99,0,0.976,0)
                                                new_list <- list(const_larve,alpha_larve,const_ponte,alpha_ponte,const_adulte,alpha_adulte)
                                                for (i in seq_len(6)){
                                                  if(is.null(new_list[[i]])){
                                                    new_list[[i]] <- defaults[[i]]
                                                  }
                                                }
                                              }else if(!(is.null(survie_adulte) & is.null(survie_larve) & is.null(proba_ponte) & is.null(proba_accoupl))){
                                                mode <- 3
                                                defaults <- c(1,4,4,4)
                                                new_list <- list(survie_adulte,survie_larve,proba_ponte,proba_accoupl)
                                                for (i in seq_len(4)){
                                                  if(is.null(new_list[[i]])){
                                                    new_list[[i]] <- defaults[[i]]
                                                  }
                                                }
                                              }else if(length(arg_uncalled) == 0){
                                                mode <- 1
                                                new_list <- 1
                                              }else if(length(arg_uncalled) == 1){
                                                if(arg_uncalled %in% 1:7){
                                                  mode <- 1
                                                  new_list <- arg_uncalled
                                                }else{
                                                  stop("un-named argument is not corresponding to code argument values\n It should be an integer between 1 & 7")
                                                }
                                              }else if(length(arg_uncalled) == 6){
                                                list_good_arg <- arg_uncalled >= 0
                                                list_good_arg[c(1,3,5)] <- list_good_arg[c(1,3,5)] & arg_uncalled[c(1,3,5)] <= 1
                                                if(all(list_good_arg)){
                                                  mode <- 2
                                                  new_list <- arg_uncalled
                                                }else{
                                                  names <- c("const_larve", "alpha_larve", "const_ponte", "alpha_ponte","const_adulte","alpha_adulte")
                                                  stop("un-named arguments are not corresponding to arguments values :\n    ",
                                                       paste(names[which(!list_good_arg)], arg_uncalled[which(!list_good_arg)], sep = ' = ',collapse = ', '))
                                                }
                                              }else{
                                                stop("Non-sens whith ",length(arg_uncalled)," un-named arguments","\n They should be 1 or 4")
                                              }
                                              if(mode == 1){
                                                if(new_list == 1){
                                                  
                                                  self$survie_adulte <- self$code(name='densite',const = 0.976, alpha = 0.002918)
                                                  self$survie_larve <- self$code("constant", const = 0.67)
                                                  self$proba_ponte <- self$code("constant", const = 0.99)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[1]
                                                }else if(new_list == 2){
                                                  
                                                  self$survie_adulte <- self$code("constant", const = 0.976)
                                                  self$survie_larve <- self$code(name='densite',const = 0.67, alpha = 0.004498)
                                                  self$proba_ponte <- self$code("constant", const = 0.99)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[2]
                                                }else if(new_list == 3){
                                                  
                                                  self$survie_adulte <- self$code("constant", const = 0.976)
                                                  self$survie_larve <- self$code("constant", const = 0.67)
                                                  self$proba_ponte <- self$code(name='densite',const = 0.99, alpha = 0.1225)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[3]
                                                }else if(new_list == 4){
                                                  
                                                  self$survie_adulte <- self$code(name='densite',const = 0.976, alpha = 0.0005)
                                                  self$survie_larve <- self$code(name='densite',const = 0.67, alpha = 0.0002354)
                                                  self$proba_ponte <- self$code("constant", const = 0.99)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[c(1,2)]
                                                }else if(new_list == 5){
                                                  
                                                  self$survie_adulte <- self$code(name='densite',const = 0.976, alpha = 0.0005)
                                                  self$survie_larve <- self$code("constant", const = 0.67)
                                                  self$proba_ponte <- self$code(name='densite',const = 0.99, alpha = 0.004055)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[c(1,3)]
                                                }else if(new_list == 6){
                                                  
                                                  self$survie_adulte <- self$code("constant", const = 0.976)
                                                  self$survie_larve <- self$code(name='densite',const = 0.67, alpha = 0.002249)
                                                  self$proba_ponte <- self$code(name='densite',const = 0.99, alpha = 0.06125)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[c(2,3)]
                                                }else if(new_list == 7){
                                                  
                                                  self$survie_adulte <- self$code(name='densite',const = 0.976, alpha = 0.000347)
                                                  self$survie_larve <- self$code(name='densite',const = 0.67, alpha = 0.00016)
                                                  self$proba_ponte <- self$code(name='densite',const = 0.99, alpha =  0.003)
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                  self$type <- list_DD[1:3]
                                                }
                                                invisible(self)
                                              }else if(mode == 2){
                                                
                                                alpha_larve <- function(x) return(x*0.004498)
                                                alpha_ponte <- function(x) return(x*0.1225)
                                                alpha_adulte <- function(x) return(x*0.002918)
                                                self$proba_accoupl <- self$code("constant", const = 0.99)
                                                self$survie_larve <- self$code(name='densite',const = new_list[[1]], alpha=alpha_larve(new_list[[2]]))
                                                self$proba_ponte <- self$code(name='densite',const = new_list[[3]], alpha=alpha_ponte(new_list[[4]]))
                                                self$survie_adulte <- self$code(name='densite', const = new_list[[5]], alpha=alpha_adulte(new_list[[6]]))
                                                
                                                self$type <- list_DD[c(new_list[[6]]!=0,2*(new_list[[2]]!=0),3*(new_list[[4]]!=0))]
                                                invisible(self)
                                              }else{
                                                if(survie_adulte == new_list[[1]]){
                                                  self$survie_adulte <- self$code(name='densite',const = 0.976, alpha = 0.0062)
                                                  self$type <- c(self$type,list_DD[1])
                                                }else if(survie_adulte == 2){
                                                  self$survie_adulte <- self$code("densite", const = 0.976, alpha = 0.01)
                                                  self$type <- c(self$type,list_DD[1])
                                                }else if(survie_adulte == 3){
                                                  self$survie_adulte <- self$code("densite", const = 0.976, alpha = 0.001)
                                                  self$type <- c(self$type,list_DD[1])
                                                }else{
                                                  self$survie_adulte <- self$code("constant", const = 0.976)
                                                }
                                                if(survie_larve == new_list[[2]]){
                                                  self$survie_larve <- self$code(name="louise",alpha=0.0000015,gamma=45)
                                                  self$type <- c(self$type,list_DD[2])
                                                }else if(survie_larve == 2){
                                                  self$survie_larve <- self$code(name="louise",alpha=0.0008,gamma=5)
                                                  self$type <- c(self$type,list_DD[2])
                                                }else if(survie_larve == 3){
                                                  self$survie_larve <- self$code(name="louise",alpha=0.002,gamma=0.06)
                                                  self$type <- c(self$type,list_DD[2])
                                                }else{
                                                  self$survie_larve <- self$code("constant", const = 0.67)
                                                }
                                                if(proba_ponte == new_list[[3]]){
                                                  self$proba_ponte <- self$code(name="louise",alpha=0.0000015,gamma=45)
                                                  self$type <- c(self$type,list_DD[3])
                                                }else if(proba_ponte == 2){
                                                  self$proba_ponte <- self$code(name="louise",alpha=0.0008,gamma=5)
                                                  self$type <- c(self$type,list_DD[3])
                                                }else if(proba_ponte == 3){
                                                  self$proba_ponte <- self$code(name="louise",alpha=0.002,gamma=0.06)
                                                  self$type <- c(self$type,list_DD[3])
                                                }else{
                                                  self$proba_ponte <- self$code("constant", const = 0.99)
                                                }
                                                if(proba_accoupl == new_list[[4]]){
                                                  self$proba_accoupl <- self$code(name="exponential",const=0.99,decalage=0,pente=0.8)
                                                  self$type <- c(self$type,list_DD[4])
                                                }else if(proba_accoupl == 2){
                                                  self$proba_accoupl <- self$code(name="exponential",const=0.99,decalage=0,pente=0.25)
                                                  self$type <- c(self$type,list_DD[4])
                                                }else if(proba_accoupl == 3){
                                                  self$proba_accoupl <- self$code(name="exponential",const=0.99,decalage=0,pente=0.15)
                                                  self$type <- c(self$type,list_DD[4])
                                                }else{
                                                  self$proba_accoupl <- self$code("constant", const = 0.99)
                                                }
                                                invisible(self)
                                              }
                                            },
                                            # permet de generer une fonction basee sur le set de fonction disponible dans le groupe private
                                            # name valant : 'constant', 'sigmoid', 'exponential', 'carre' or 'densite' appelant une fonction nommee en rapport a sa forme
                                            # const est le point maximum de la fonction, obtenus en decalage a l'infini ou en 0
                                            # decalage permet de deplacer la courbe le long de l'axe des abscices quand il est disponible
                                            # pente change la force de la pente 
                                            # alpha et gamma provienne de la fonction densite
                                            code = function(name='constant',const=1,decalage=0,pente=0,alpha=0,gamma=1){
                                              if(!exists(name,private)){
                                                stop("name should be :\n 'constant', 'sigmoid', 'exponential', 'carre' or 'densite'")
                                              }
                                              return(function(x) get(name,private)(x,const,decalage,pente,alpha,gamma))
                                            },
                                            # lorsque l'on applique param[x,i] cela renvoie une dataframe de length(x)+1 lignes et length(i) colonnes
                                            # chaque ligne j est l'application de la j eme valeur de x par les i espaces de stockages
                                            `[` = function(x,colonne){
                                              res <- data.frame(x = x,
                                                                survie_adulte = self$survie_adulte(x),
                                                                survie_larve = self$survie_larve(x),
                                                                proba_ponte = self$proba_ponte(x),
                                                                proba_accoupl = self$proba_accoupl(x))
                                              return(res[colonne])
                                            },
                                            # permet d'afficher les fonctions du fonction.frame
                                            plot = function(which = 1:4, xlim=c(0,1000),legend_show=T,lty=1){
                                              plot(y=-1,x=-1,xlim=xlim,ylim=c(0,1),xlab='x',ylab='value')
                                              data <- self[xlim[1]:xlim[2]]
                                              legend <- c()
                                              col <- c()
                                              for (i in which){
                                                legend <- c(legend,names(data)[i+1])
                                                col <- c(col,i+1)
                                                points(y = data[[i+1]],x=xlim[1]:xlim[2],type='l',col=i+1,lwd=2,lty=lty)
                                              }
                                              if(legend_show){
                                                legend("topright",inset=0.05, legend=legend,col=col,lwd=2,lty=lty)
                                              }
                                            },
                                            points = function(which = 1:4,xlim=c(0,1000),lty=1){
                                              data <- self[xlim[1]:xlim[2]]
                                              for (i in which){
                                                points(y = data[[i+1]],x=xlim[1]:xlim[2],type='l',col=i+1,lwd=2,lty=lty)
                                              }
                                            }
                              ),
                              # definition des fonctions de references
                              ############### Principales
                              private=list(constant = function(x,const=1,decalage=0,pente=0,alpha=0,gamma=1) return(rep(const,length(x))),
                                           densite = function(x,const=1,decalage=0,pente=0,alpha=0,gamma=1) return(const/(1+(alpha*x)**gamma)),
                                           louise = function(x,const=1,decalage=0,pente=0,alpha=0,gamma=1) if (gamma != 0){const*x/((1+alpha*x)*(gamma+x))}else{const/(1+alpha*x)},
                                           exponential = function(x,const=1,decalage=0,pente=0,alpha=0,gamma=1) return(exp(-abs(pente)/100*x)*(decalage-const)+const),
                                           
                                           ## Suplementaires
                                           sigmoid = function(x,const=1,decalage=0,pente=0,alpha=0,gamma=1) return(const*(1-1/(1+exp((x-decalage)*pente/100)))),
                                           carre = function(x,const=1,decalage=0,pente=0,alpha=0,gamma=1) return(const*2/(1+exp(((x-decalage)*pente/100)**2)))
                              )
)

## Importe un set de densite dependence
parametres.dens_dep <- parametres.mobiles$new(const_larve = 0.67,
                                              alpha_larve = 0.5,
                                              const_ponte = 0.99,
                                              alpha_ponte = 0.5,
                                              const_adulte = 0.976,
                                              alpha_adulte = 0)

##Fonction qui cree une list de situation de densite dependence
list_of_dens_dep <- function(code = 1:7){
  if(is.numeric(code)){
    list_DD <- rep(list(NULL),length(code))
    for(i in seq_len(length(code))){
      list_DD[[i]] <- parametres.mobiles$new(code = code[[i]])
    }
  }else if(is.data.frame(code)){
  
    list_DD <- rep(list(NULL),nrow(code))
    if(is.null(code$const_larve)){
      code$const_larve <- 0.67
    }
    if(is.null(code$alpha_larve)){
      code$alpha_larve <- 0.5
    }
    if(is.null(code$const_ponte)){
      code$const_ponte <- 0.99
    }
    if(is.null(code$alpha_ponte)){
      code$alpha_ponte <- 0.5
    }
    if(is.null(code$const_adulte)){
      code$const_adulte <- 0.976
    }
    if(is.null(code$alpha_adulte)){
      code$alpha_adulte <- 0
    }
    for (i in 1:nrow(code)){
      list_DD[[i]] <- parametres.mobiles$new(const_larve = code$const_larve[i],
                                             alpha_larve = code$alpha_larve[i],
                                             const_ponte = code$const_ponte[i],
                                             alpha_ponte = code$alpha_ponte[i],
                                             const_adulte = code$const_adulte[i],
                                             alpha_adulte = code$alpha_adulte[i])
    }
  }else{
    stop("Code has a wrong format")
  }
  return(list_DD)
}

###################################
###    FIN CODAGE PARAMETRES    ###
###################################