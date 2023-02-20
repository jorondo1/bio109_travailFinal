# Travail final BIO109
# Auteurs: Jonathan Rondeau-Leclaire, Chloé Girard

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
