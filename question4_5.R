source("question1.R")
source("question2.R")
source("question3.R")

###############################################################################
# 4. Exécuter la fonction sur 10 et 100 pas de temps, #########################
#### et évaluer le pas de calcul avec system.time().  #########################
###############################################################################

simulation_10 <- simul.fun(10,etat_0)
simulation_100 <- simul.fun(100,etat_0)

system.time(simul.fun(10,etat_0))
system.time(simul.fun(100,etat_0))

###############################################################################
# 5. Questions à répondre  ####################################################
###############################################################################

# 5i) qu'advient-il à 25, 50, 75 ans? #########################################

# Pusiqu'un pas de temps est de 5 ans, on cherche les pas 5, 10 et 15
subset(simulation_100, 
       subset = grepl("M", etats),  # sélectionner la rangée M
       select = c("pas_5", "pas_10", "pas_15")) # extraire les colonnes d'intérêt

# Calculer la composition de l'état de départ, pour comparer:
(comp.etat0 <- table(etat_0)/500)

###! RÉPONSE
# Au début, la forêt mixte constitue environ 75,2 % des quadrats. Elle décroit
# graduellement; après 25 ans, 37,6 % des quadrats en sont constitués;
# après 50 ans, 23,8 %; et finalement, 17,6 % des quadrats après 75 ans.

# 5ii) qu'advient-il à 75 ans?

### On suppose une forêt de 500 quadrats de fôret mixte au pas de temps 0
foretMixte <- factor(rep("M",500), levels=etats)

### exécuter la simulation sur cette forêt (15 pas de temps de 5 ans chacun)
simulation_mixte <- simul.fun(15, foretMixte)
simulation_mixte[,15] # le 15e pas de temps qui nous intéresse

###! RÉPONSE
# Après 75 ans (15 pas de temps), la forêt devient 22,0 % mixte, 69,8 % tempérée,
# et 8,2 % "rien".

# 5iii) Proportion de la parcelle occupée par de la forêt tempérée
simulation_75 <- simul.fun(75,etat_0)

plot_data <- subset(simulation_75, 
       subset = grepl("T", etats), # extraire les proportions de forêt tempérée
       select = c("pas_2", "pas_5", "pas_10", "pas_15")) # et les pas d'intérêt

plot_data <- c(comp.etat0[["T"]], # l'état initial
               plot_data) # les états suivants
  
barplot(plot_data, xlab = "Années", ylab = "Proportion des quadrats",
          main = "Projection de la proportion des quadrats occupés par de la forêt tempérée",
          names.arg = c("0 (état initial)","10 ", "25 ", "50 ", "75"), ylim=c(0,1),
          col = "forestgreen")




