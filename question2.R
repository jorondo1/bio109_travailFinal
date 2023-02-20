# Travail final BIO109
# Auteurs: Jonathan Rondeau-Leclaire, Chloé Girard

###############################################################################
# 2. Fonction pour tirer au hasard l'état t+1 ################################
###############################################################################

etat.fun <- function(etat) { # la fonction prend comme argument un état (ex. "M")
  p <- transMx[etat,] # sélection du vecteur de probabilité associé à l'état
  res <- rmultinom(n=1, size=1, prob = p) # tirage aléatoire à distribution multinomiale  
  return(etats[which(res==1)]) # on retourne le nom de l'état associé
}
