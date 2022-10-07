
###########################
### FONCTIONS DES PLOTS ###
###########################

plot_by_variable <- function(.data,variable, type = 'l', save = F, name = NULL, path ='.'){
  border <- function(vector,min=0,max=1){ifelse(vector>max,max,ifelse(vector<min,min,vector))}
  variable <- enquo(variable)
  options(dplyr.summarise.inform = FALSE)
  resume <- .data %>% group_by(!!variable) %>% 
    summarise(repetition = n(), probabilite = mean(envahie),
              erreur_probabilite=2.567*sqrt(probabilite*(1-probabilite)/repetition),
              temps = mean(temps_50,na.rm = T),
              erreur_temps = 2.567*sd(temps_50,na.rm = T)/sqrt(repetition),
              densite_min = border(mean(diminution_relative,na.rm = T)),
              erreur_diminution = 2.567*sd(diminution_relative,na.rm = T)/sqrt(repetition))
  options(dplyr.summarise.inform = TRUE)
  plot_proba <- ggplot(resume, aes(x=!!variable,
                                   y=probabilite,
                                   ymin=border(probabilite - erreur_probabilite), 
                                   ymax=border(probabilite + erreur_probabilite)))
  
  plot_temps <- ggplot(resume, aes(x=!!variable,
                                   y=temps,
                                   ymin=temps - erreur_temps,
                                   ymax=temps + erreur_temps))
  
  
  plot_densite <- ggplot(resume, aes(x=!!variable,
                                     y=densite_min,
                                     ymin=border(densite_min - erreur_diminution),
                                     ymax=border(densite_min + erreur_diminution)))
  
  if(type=='p'){
    plot_proba <- plot_proba + 
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
    plot_temps <- plot_temps +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
    plot_densite <- plot_densite +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
  }else{
    plot_proba <- plot_proba + 
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
    plot_temps <- plot_temps +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
    plot_densite <- plot_densite +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
  }
  
  plot_proba <- plot_proba + 
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none")
  plot_temps <- plot_temps +
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none")
  plot_densite <- plot_densite +
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none")
  
  plot_proba <- plot_proba + theme(legend.position = 'none')
  plot_temps <- plot_temps + theme(legend.position = 'none')
  plot_densite <- plot_densite + theme(legend.position = 'none')
  
  
  plot_final <- ggdraw() +
    draw_plot(plot_proba, 0, 0.1, 0.333, 0.9) +
    draw_plot(plot_temps, 0.333, 0.1, 0.333, 0.9) +
    draw_plot(plot_densite, 0.666, 0.1, 0.333, 0.9) +
    draw_plot_label(c("A", "B", "C"), c(0, 0.333, 0.666), c(1, 1, 1), size = 15)
  
  if(save){
    if(missing(name)){
      name0 <- ensym(variable)
    }else{
      name0 <- name
    }
    old_dir <- getwd()
    setwd(path)
    i <- 0
    name <- paste(name0,i,sep='_')
    while(paste0(name,'.pdf') %in% list.files()){
      name <- paste(name0,i,sep='_')
      i <- i+1
    }
    save_plot(paste0(name,'.pdf'), plot_final,
              base_aspect_ratio = 1.5,
              base_height = 11)
    setwd(old_dir)
  }
  return(plot_final)
}

plot_by_DD <- function(table, variable, split=T, DD_codes = 1:7, type = 'l', save = F, name = NULL, path ='.'){
  border <- function(vector,min=0,max=1){ifelse(vector>max,max,ifelse(vector<min,min,vector))}
  var_names <- c("set_parametre","set_densite_dep",
                 "nb_couple_init","quantite_introduite",
                 "nombre_introduction","male_ratio",
                 "mated_ratio","premiere_introduction",
                 "interval_intro","envahie",
                 "temps_50","diminution_relative")
  if(!is.data.frame(table)){
    stop("table argument should be a data.frame")
  }
  if(!all(var_names %in% names(table))){
    stop(paste("Some collumns are missing in table argument :",paste(var_names[which(!var_names %in% names(table))],collapse = ', ')))
  }
  list_DD <- c('Adultes', 'Larves', 'Pontes', 'Adultes_Larves',
               'Adultes_Pontes','Larves_Pontes','Adultes_Larves_Pontes')
  DD_presente <- list_DD %in% table$set_densite_dep
  if(any(DD_presente)){
    table$set_densite_dep <- factor(table$set_densite_dep, levels = list_DD[which(DD_presente)])
  }else{
    table$set_densite_dep <- factor(table$set_densite_dep)
    list_DD <- levels(table$set_densite_dep)
  }
  modalities <- which(DD_presente)
  if(any(DD_codes %in% modalities)){
    DD_codes <- DD_codes[which(DD_codes %in% modalities)]
  }else{
    stop('DD_codes do no coincide with set_densite_dep argument in table')
  }
  variable <- enquo(variable)
  options(dplyr.summarise.inform = FALSE)
  table_extract <- table %>% filter(set_densite_dep %in% list_DD[DD_codes]) %>%
    group_by(set_densite_dep,!!variable) %>% summarise(repetition = n(), probabilite = mean(envahie),  erreur_probabilite=2.567*sqrt(probabilite*(1-probabilite)/repetition),
                                                       temps = mean(temps_50,na.rm = T),erreur_temps = 2.567*sd(temps_50,na.rm = T)/sqrt(repetition),
                                                       densite_min = border(mean(diminution_relative,na.rm = T)),erreur_diminution = 2.567*sd(diminution_relative,na.rm = T)/sqrt(repetition))
  options(dplyr.summarise.inform = TRUE)
  nb_points <- dim(table_extract)[1] / length(DD_codes)
  if(floor(nb_points) != nb_points){
    stop("Wrong table organisation !!! Problem with set_densite_dep in table")
  }
  if(nb_points == 1){
    type <- 'p'
  }
  if(split){
    plot_proba <- ggplot(table_extract, aes(x=!!variable, 
                                            y=probabilite,
                                            ymin=border(probabilite - erreur_probabilite), 
                                            ymax=border(probabilite + erreur_probabilite))) +
      facet_grid(. ~ set_densite_dep)
    plot_temps <- ggplot(table_extract, aes(x=!!variable,
                                            y=temps,
                                            ymin=temps - erreur_temps,
                                            ymax=temps + erreur_temps)) +
      facet_grid(. ~ set_densite_dep)
    plot_densite <- ggplot(table_extract, aes(x=!!variable,
                                              y=densite_min,
                                              ymin=border(densite_min - erreur_diminution),
                                              ymax=border(densite_min + erreur_diminution))) +
      facet_grid(. ~ set_densite_dep)
  }else{
    plot_proba <- ggplot(table_extract, aes(x=!!variable, 
                                            y=probabilite,
                                            ymin=border(probabilite - erreur_probabilite), 
                                            ymax=border(probabilite + erreur_probabilite),
                                            colour = set_densite_dep, fill=set_densite_dep))
    plot_temps <- ggplot(table_extract, aes(x=!!variable,
                                            y=temps,
                                            ymin=temps - erreur_temps,
                                            ymax=temps + erreur_temps,
                                            colour = set_densite_dep,fill=set_densite_dep))
    plot_densite <- ggplot(table_extract, aes(x=!!variable,
                                              y=densite_min,
                                              ymin=border(densite_min - erreur_diminution),
                                              ymax=border(densite_min + erreur_diminution),
                                              colour = set_densite_dep,fill=set_densite_dep))
  }
  if(type=='p'){
    plot_proba <- plot_proba + 
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
    plot_temps <- plot_temps +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
    plot_densite <- plot_densite +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
  }else{
    plot_proba <- plot_proba + 
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
    plot_temps <- plot_temps +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
    plot_densite <- plot_densite +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
  }
  legend <- get_legend(plot_proba + theme(legend.position = 'bottom'))
  plot_proba <- plot_proba + 
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none")
  plot_temps <- plot_temps +
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none")
  plot_densite <- plot_densite +
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none")
  if(split){
    plot_proba <- plot_proba + theme(axis.title.x=element_blank())
    plot_temps <- plot_temps + theme(axis.title.x=element_blank())
    plot_final <- ggdraw() +
      draw_plot(plot_proba, 0, 0.675, 1, 0.325) +
      draw_plot(plot_temps, 0, 0.35, 1, 0.325) +
      draw_plot(plot_densite, 0, 0, 1, 0.35) +
      draw_plot_label(c("A", "B", "C"), c(0, 0, 0), c(1, 0.675, 0.35), size = 15)
  }else{
    plot_proba <- plot_proba + theme(legend.position = 'none')
    plot_temps <- plot_temps + theme(legend.position = 'none')
    plot_densite <- plot_densite +
      theme(legend.position = 'none')
    plot_final <- ggdraw() +
      draw_plot(plot_proba, 0, 0.1, 0.333, 0.9) +
      draw_plot(plot_temps, 0.333, 0.1, 0.333, 0.9) +
      draw_plot(plot_densite, 0.666, 0.1, 0.333, 0.9) +
      draw_plot(legend, 0, 0, 1, 0.1) +
      draw_plot_label(c("A", "B", "C"), c(0, 0.333, 0.666), c(1, 1, 1), size = 15)
  }
  if(save){
    if(missing(name)){
      name0 <- ensym(variable)
    }else{
      name0 <- name
    }
    old_dir <- getwd()
    setwd(path)
    i <- 0
    name <- paste(name0,i,sep='_')
    while(paste0(name,'.pdf') %in% list.files()){
      name <- paste(name0,i,sep='_')
      i <- i+1
    }
    if(split){
      save_plot(paste0(name,'.pdf'), plot_final,
                base_aspect_ratio = 1.5,
                base_height = 11)
    }else{
      save_plot(paste0(name,'.pdf'), plot_final,
                base_aspect_ratio = 1.5,
                base_height = 11)
    }
    
    setwd(old_dir)
  }
  return(plot_final)
}

plot_by_DD2 <- function(table, variable, DD_codes = 1:7,ref = 'Ninf', type = 'l', save = F, name = NULL, path ='.'){
  border <- function(vector,min=0,max=1){ifelse(vector>max,max,ifelse(vector<min,min,vector))}
  var_names <- c("set_parametre","set_densite_dep",
                 "nb_couple_init","quantite_introduite",
                 "nombre_introduction","male_ratio",
                 "mated_ratio","premiere_introduction",
                 "interval_intro","envahie",
                 "temps_50","diminution_relative")
  if(!is.data.frame(table)){
    stop("table argument should be a data.frame")
  }
  if(!all(var_names %in% names(table))){
    stop(paste("Some collumns are missing in table argument :",paste(var_names[which(!var_names %in% names(table))],collapse = ', ')))
  }
  list_DD <- c('Adultes', 'Larves', 'Pontes', 'Adultes_Larves',
               'Adultes_Pontes','Larves_Pontes','Adultes_Larves_Pontes')
  DD_presente <- list_DD %in% table$set_densite_dep
  if(any(DD_presente)){
    table$set_densite_dep <- factor(table$set_densite_dep, levels = list_DD[which(DD_presente)])
  }else{
    table$set_densite_dep <- factor(table$set_densite_dep)
    list_DD <- levels(table$set_densite_dep)
  }
  modalities <- which(DD_presente)
  if(any(DD_codes %in% modalities)){
    DD_codes <- DD_codes[which(DD_codes %in% modalities)]
  }else{
    stop('DD_codes do no coincide with set_densite_dep argument in table')
  }
  variable <- enquo(variable)
  options(dplyr.summarise.inform = FALSE)
  table_extract <- table %>% filter(set_densite_dep %in% list_DD[DD_codes]) %>%
    group_by(set_parametre,set_densite_dep,!!variable) %>% summarise(repetition = n(), probabilite = mean(envahie),  erreur_probabilite=2.567*sqrt(probabilite*(1-probabilite)/repetition),
                                                                     temps = mean(temps_50,na.rm = T),erreur_temps = 2.567*sd(temps_50,na.rm = T)/sqrt(repetition),
                                                                     densite_min = border(mean(diminution_relative,na.rm = T)),erreur_diminution = 2.567*sd(diminution_relative,na.rm = T)/sqrt(repetition))
  options(dplyr.summarise.inform = TRUE)
  which_ref <- which(table_extract$set_parametre %in% ref)
  table_extract$probabilite[which_ref] <- NA
  table_extract$temps[which_ref] <- NA
  
  nb_points <- dim(table_extract)[1] / length(DD_codes)
  if(floor(nb_points) != nb_points){
    stop("Wrong table organisation !!! Problem with set_densite_dep in table")
  }
  if(nb_points == 1){
    type <- 'p'
  }
    plot_proba <- ggplot(table_extract, aes(x=!!variable,
                                            y=probabilite,
                                            ymin=border(probabilite - erreur_probabilite),
                                            ymax=border(probabilite + erreur_probabilite),
                                            color = set_parametre, fill = set_parametre)) +
      facet_grid(. ~ set_densite_dep)
    plot_temps <- ggplot(table_extract, aes(x=!!variable,
                                            y=temps,
                                            ymin=temps - erreur_temps,
                                            ymax=temps + erreur_temps,
                                            color = set_parametre, fill = set_parametre)) +
      facet_grid(. ~ set_densite_dep)
    plot_densite <- ggplot(table_extract, aes(x=!!variable,
                                              y=densite_min,
                                              ymin=border(densite_min - erreur_diminution),
                                              ymax=border(densite_min + erreur_diminution),
                                              color = set_parametre, fill = set_parametre)) +
      facet_grid(. ~ set_densite_dep)
    
  if(type=='p'){
    plot_proba <- plot_proba +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
    plot_temps <- plot_temps +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
    plot_densite <- plot_densite +
      geom_point(na.rm = T) +
      geom_errorbar(position=position_dodge(0.05))
  }else{
    plot_proba <- plot_proba +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
    plot_temps <- plot_temps +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
    plot_densite <- plot_densite +
      geom_line(na.rm = T) +
      geom_ribbon(alpha=0.1,colour=NA)
  }
  legend <- get_legend(plot_proba + theme(legend.position = 'bottom'))
  plot_proba <- plot_proba + 
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none") +
    theme(legend.position = 'none') 
  plot_temps <- plot_temps +
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none") +
    theme(legend.position = 'none') 
  plot_densite <- plot_densite +
    panel_border()+
    theme_bw() +
    background_grid(major = 'y', minor = "none") +
    theme(legend.position = 'none') 
  plot_proba <- plot_proba + theme(axis.title.x=element_blank())
  plot_temps <- plot_temps + theme(axis.title.x=element_blank())
  plot_final <- ggdraw() +
    draw_plot(plot_proba, 0, 0.6875, 1, 0.3125) +
    draw_plot(plot_temps, 0, 0.375, 1, 0.3125) +
    draw_plot(plot_densite, 0, 0.04, 1, 0.335) +
    draw_plot(legend, 0, 0, 1, 0.04) +
    draw_plot_label(c("A", "B", "C",""), c(0, 0, 0, 0), c(1, 0.675, 0.35,0), size = 15)
  
  if(save){
    if(missing(name)){
      name0 <- ensym(variable)
    }else{
      name0 <- name
    }
    old_dir <- getwd()
    setwd(path)
    i <- 0
    name <- paste(name0,i,sep='_')
    while(paste0(name,'.pdf') %in% list.files()){
      name <- paste(name0,i,sep='_')
      i <- i+1
    }
      save_plot(paste0(name,'.pdf'), plot_final,
                base_aspect_ratio = 1.5,
                base_height = 11)
    setwd(old_dir)
  }
  return(plot_final)
}

