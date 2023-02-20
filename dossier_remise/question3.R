# Travail final BIO109
# Auteurs: Jonathan Rondeau-Leclaire, Chloé Girard

###############################################################################
# 3. Fonction de simulation rapportant la composition sur N pas de temps #####
###############################################################################

simul.fun <- function(N,        # nombre de pas de temps désiré
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