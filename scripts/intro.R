

#Création des variables d'etudes
taille <- rnorm(500, 1.7, 0.4)
poids <- rnorm(500, 55, 10)
age <- rnorm(500, 21, 5)


#Affichage
plot(taille~poids)
plot(taille~poids, pch = 16, col = "red",
     main = "Relation entre Pois et Taille")


#Création du dataframe
dataset <- as.data.frame(cbind(taille = taille, poids = poids,
                               age = age))


summary(dataset)
library(gs)