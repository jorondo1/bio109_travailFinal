###############################################################################
### 1. Transformation des abondances en états de départ #######################
###############################################################################

# Conditions des différents états de la forêt
# 'B' si présence de abba ou piru
# 'T' si présence de acsa ou acpe ou beal ou fagr
# 'M' si présence de (abba ou piru) et (acsa ou acpe ou beal ou fagr)
# 'R' si aucun de ces critères n'est rempli

library(pacman)
p_load(tidyverse)

quadrats <- read.csv2("quadrats.txt")


# états possibles de la forêt
etats <- c("B","M","T","R")

etat_0 <- factor(
  ifelse((quadrats$abba > 0 | quadrats$piru > 0) & 
           quadrats$acsa==0 & quadrats$beal==0 & 
           quadrats$acpe==0 & quadrats$fagr==0, "B",
         ifelse(quadrats$abba==0 & quadrats$piru==0 & 
                  (quadrats$acsa>0 | quadrats$beal>0 | 
                     quadrats$acpe>0 | quadrats$fagr>0), "T",
                ifelse((quadrats$abba > 0 | quadrats$piru > 0) & 
                         (quadrats$acsa>0 | quadrats$beal>0 | 
                            quadrats$acpe>0 | quadrats$fagr>0), "M",
                       "R"))), 
  levels=etats) # pour s'assurer que les facteurs gardent une indexation constante,
                # évite un bogue dans la prédiction lorsqu'on prédit un pas de temps
                # où un état est absents

summary(etatsMx)

###############################################################################
# 2. Calcul de l'état t+1 #####################################################
###############################################################################

# Matrice de transition entre les différents états
# Probabilité d'obtenir un état au pas t+1 étant donné l'état au pas de tempst.
transMx <- read.table("matrice_transitions.txt") 

# on nomme les rangées et colonnes pour se faciliter la tâche visuellement:
rownames(transMx) <- etats 
colnames(transMx) <- etats
transMx

###############################################################################
# 3a. Fonction pour tirer au hasard l'état t+1 ################################
###############################################################################

etat.fun <- function(etat) { # la fonction prend comme argument un état (ex. "M")
  p <- transMx[etat,] # sélection du vecteur de probabilité associé à l'état
  res <- rmultinom(n=1, size=1, prob = p) # tirage aléatoire à distribution multinomiale  
  return(etats[which(res==1)]) # on retourne le nom de l'état associé
}

###############################################################################
# 3b. Fonction de simulation rapportant la composition sur N pas de temps #####
###############################################################################

simul.fun2 <- function(N,        # nombre de pas de temps désiré
                       pas_0){   # vecteur de l'état initial
  set.seed(123) # reproductibilité!
  
  # création d'une matrice 4xN qui portera la composition par état pour chaque pas de temps:
  composition <- matrix(nr = length(etats), nc = N)
  
  pas_t <- pas_0 # extraire le vecteur du pas t
  
  liste_pas <- matrix(nr = 1, nc = N) # initialiser une matrice de N colonnes

  for (i in 1:(N)){
    # On doit d'abord créer un vecteur pour le pas de temps t+1   
    liste <- list()  # initialiser une liste vide
    
    for (j in 1:length(pas_t)){ # populer la liste par itération
      liste[[j]] <- etat.fun(pas_t[j]) # prédiction de l'état t+1
    }
    
    pas_plus1 <- factor(unlist(liste), levels = etats)  # l'ajouter à la matrice
    
    # Puis on extrait la composition du pas t+1 avec la fonction table()
    comp_t1 <- as.vector(table(pas_plus1)) 
    
    composition[,i] <- comp_t1 # sauvegarder la composition dans la matrice
    liste_pas[,i] <- paste0("pas_",i) # titre de colonne
    
    # Finalement, le pas t+1 devient le pas t pour l'itération suivante:
    pas_t <- pas_plus1
  }
  rownames(composition) <- etats       # on aime ça beau
  colnames(composition) <- liste_pas
  return(composition/length(pas_0)) # on divise par le nombre total de quadrats pour
  # obtenir la proportion de la forêt complète.
}

simulation_10 <- simul.fun2(10,etat_0)
simulation_100 <- simul.fun2(100,etat_0)

###############################################################################
# 4. Exécuter la fonction sur 10 et 100 pas de temps, #########################
#### et évaluer le pas de calcul avec system.time().  #########################
###############################################################################

system.time(simul.fun2(10,etat_0))
system.time(simul.fun2(100,etat_0))

###############################################################################
# 5. Questions à répondre
###############################################################################


# i) qu'advient-il à 25, 50, 75ans?
### Pusique le pas de temps est de 5 ans, on cherche les pas de temps 5, 10 et 15
subset(simulation_100, 
       subset = grepl("M",etats),  # sélectionner la rangée M
       select = c("pas_5", "pas_10", "pas_15")) # extraire les colonnes d'intérêt

# ii) qu'advient-il à 75 ans?

### On suppose une forêt de 500 quadrats de fôret mixte au pas de temps 0
foretMixte <- factor(rep("M",500), levels=etats)

### exécuter la simulation sur cette forêt
(simulation_mixte <- simul.fun2(15, foretMixte))
### après 75 ans (15 pas de temps), la forêt devient 22% mixte, 69,8% tempérée,
### et 8,2% "rien".

# iii) 
foretTemp <- factor(rep("T",500), levels=etats)
simulation_temp <- simul.fun2(15, foretTemp)
subset(simulation_temp, select = c("pas_2", "pas_5", "pas_10", "pas_15")) |>
  barplot()

```