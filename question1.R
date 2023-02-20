# Travail final BIO109
# Auteurs: Jonathan Rondeau-Leclaire, Chloé Girard

###############################################################################
### 1. Transformation des abondances en états de départ #######################
###############################################################################

# Conditions des différents états de la forêt
# 'B' si présence de abba ou piru
# 'T' si présence de acsa ou acpe ou beal ou fagr
# 'M' si présence de (abba ou piru) et (acsa ou acpe ou beal ou fagr)
# 'R' si aucun de ces critères n'est rempli

# Chargement des données
quadrats <- read.csv2("quadrats.txt")

# États possibles de la forêt
etats <- c("B","M","T","R")

# Création d'un vecteur représentant l'état au temps 0 en fonction de la 
# composition de la forêt:
etat_0 <- factor(
  ifelse((quadrats$abba > 0 | quadrats$piru > 0) & 
           quadrats$acsa == 0 & quadrats$beal == 0 & 
           quadrats$acpe == 0 & quadrats$fagr == 0, "B",
         ifelse(quadrats$abba == 0 & quadrats$piru == 0 & 
                  (quadrats$acsa>0 | quadrats$beal>0 | 
                     quadrats$acpe>0 | quadrats$fagr>0), "T",
                ifelse((quadrats$abba > 0 | quadrats$piru > 0) & 
                         (quadrats$acsa>0 | quadrats$beal>0 | 
                            quadrats$acpe>0 | quadrats$fagr>0), "M",
                       "R"))), 
  levels=etats) # pour s'assurer que les facteurs gardent une indexation constante,
# évite un bogue dans la prédiction lorsqu'on prédit un pas de temps
# où un état est absents
