


#Install libraries
install.packages(c("pacman", "tidyverse", "rio"))

#load libraries
library(tidyverse)
library(sf)


data <- read.table(file = "data/Data_Example_1.txt", header = TRUE, sep = "\t")
data$sampleID <- as.numeric(data$sampleID)
data$subplot <- as.numeric(data$subplot)
data$OTU_all_taxa <- as.numeric(data$OTU_all_taxa)

# Créer des sous-ensembles de données pour June
june <- data[data$month == "June", ]

# Spécifier les colonnes x et y comme coordonnées
coordinates(june) <- ~x + y

# Calculer le variogramme empirique pour novembre
var1 <- variogram((log(OTU_all_taxa) * 10) ~ 1, locations = june, cutoff = 8, width = 1)

# Ajuster le modèle de variogramme
mod1 <- fit.variogram(var1, vgm(psill = NA, c("Exp", "Sph"), range = NA, 1), fit.sills = TRUE, fit.ranges = TRUE, fit.method = 1)

# Validation croisée pour évaluer la précision du modèle
cv <- krige.cv((log(OTU_all_taxa) * 10) ~ 1, locations = june, model = mod1, nfold = nrow(june)) # cv : Corss validation

cv_df <- as.data.frame(cv)

rio::export(cv_df, "docs/cv_agathe", format = "xlsx")
cv_fl <- flextable::flextable(cv_df)
cv_fl
#Creons une fonction pour calculer l'erreur moyenne

EM <- function(effectif, zcore){
  sum(zcore)/effectif
}

EM(nrow(cv), zcore = cv$zscore)

#creons un dataframme avec EM comme colonne
effectif <- nrow(cv)
