library(dplyr)

######################################
# LECTURE FICHIER DE DONNEES
fichier_voyelles_ESTER_NCCFr = "https://bigdataspeech.github.io/TP/fichiers/acoustique_voy_orales_20loc_ESTER_NCCFr_contexte_freqLex_distCentroide.txt"
dataset = read.table(file = fichier_voyelles_ESTER_NCCFr, sep="\t", header = T, fileEncoding = "UTF-8", quote= "", comment.char = "")

######################################
# CREATION DES TABLEAUX RECAPITULATIFS
datasetBySpk = group_by(dataset, locuteur)
datasetBySpk = summarise(
  datasetBySpk,
  F1moy = mean(F1),
  F2moy = mean(F2),
  Z1moy = mean(Z1),
  Z2moy = mean(Z2)
)

datasetByVowel = group_by(dataset, voyelle)
datasetByVowel = summarise(
  datasetByVowel,
  F1moy = mean(F1),
  F2moy = mean(F2),
  Z1moy = mean(Z1),
  Z2moy = mean(Z2)
)

datasetBySpkVowel = group_by(dataset, locuteur, voyelle)
datasetBySpkVowel = summarise(
  datasetBySpkVowel,
  F1moy = mean(F1),
  F2moy = mean(F2),
  Z1moy = mean(Z1),
  Z2moy = mean(Z2)
)
# Ajout d'une colonne avec les 2 variables catégorielles concaténées, et conversion au format factor
datasetBySpkVowel$locuteur_voyelle = paste0(datasetBySpkVowel$locuteur, "_", datasetBySpkVowel$voyelle)
datasetBySpkVowel$locuteur_voyelle = as.factor(datasetBySpkVowel$locuteur_voyelle)

######################################
# CLUSTERING HIERARCHIQUE

# Choix de la méthode
clusteringMethod = "ward.D"
# clusteringMethod = "ward.D2"
# clusteringMethod = "single"
# clusteringMethod = "complete"
# clusteringMethod = "average"
# clusteringMethod = "mcquitty"
# clusteringMethod = "median"
# clusteringMethod = "centroid"

# Définition du jeu de données et du critère de comparaison correspondant
targetDataset = datasetBySpk
colLabels = "locuteur"
# targetDataset = datasetByVowel
# colLabels = "voyelle"
# targetDataset = datasetBySpkVowel
# colLabels = "locuteur_voyelle"

# Colonnes à utiliser pour le calcul des distances par paire (mesures de formants en Hz ou Bark)
colF1 = "F1moy"
colF2 = "F2moy"
# colF1 = "Z1moy"
# colF2 = "Z2moy"

# Commandes pour le clustering
# Création de la matrice de distances
distMat = dist(targetDataset[,c(colF1, colF2)])
# Clustering
hclustObj = hclust(distMat, method = clusteringMethod)
# Tracé du dendrogramme
plot(hclustObj, labels = as.character(targetDataset[[colLabels]]))
# Export du groupe auquel chaque élément appartient en fonction du niveau auquel on coupe l'arbre (valeur entre 2 et N-1)
cutLevel = 20
targetDataset[,paste0("groupMembership",cutLevel)] = cutree(hclustObj, cutLevel)
