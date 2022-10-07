rm(list=ls(all.names=T))

# TRUE ou FALSE selon le model desire
stochastique = T
source("Programme/Simulations_Modele.R")


data1 <- read.csv2("II - Resultats/VI - effet constantes de survie/effet_const_ponte.csv")

data1 %>% plot_by_variable(const_ponte, type = 'l', save = T, path ="II - Resultats/VI - effet constantes de survie")


data2 <- read.csv2("II - Resultats/VI - effet constantes de survie/effet_const_adulte.csv")

data2 %>% plot_by_variable(const_adulte, type = 'l', save = T, path ="II - Resultats/VI - effet constantes de survie")



data3 <- read.csv2("II - Resultats/VI - effet constantes de survie/effet_const_larve.csv")

data3 %>% plot_by_variable(const_larve, type = 'l', save = T, path ="II - Resultats/VI - effet constantes de survie")
