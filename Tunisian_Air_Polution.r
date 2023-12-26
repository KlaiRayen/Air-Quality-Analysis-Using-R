####Installation et importation des packages
#install.packages("naniar")
#install.packages("Hmisc")
#install.packages("VIM")
#install.packages("FactoMineR")
#install.packages("factoextra")
library(VIM)
library(Hmisc)
library(lubridate)
library(dplyr)
library(naniar)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(mgcv)
library(corrplot)

###Importation des données 
Tozeur = read.table(file = file.choose(),header=T,sep=",",dec=".")
Nabeul = read.table(file = file.choose(),header=T,sep=",",dec=".")
Jerba = read.table(file = file.choose(),header=T,sep=",",dec=".")
View(Tozeur)
str(Tozeur)
#supprimer la 1ere ligne
Tozeur <- Tozeur[-1, ]
View(Tozeur)
dim(Tozeur)
View(Nabeul)
str(Nabeul)
dim(Nabeul)
View(Jerba)
str(Jerba)
#supprimer la 1ere ligne
Jerba <- Jerba[-1, ]
View(Jerba)
dim(Jerba)
#Ajout d'une colonne qui correspond aux région
Tozeur$region = "Tozeur"
Jerba$region = "Jerba"
Nabeul$region = "Nabeul"
#Combiner les 3 datasets dans une seule
Data = rbind(Tozeur, Jerba, Nabeul)
dim(Data)
View(Data)
str(Data)
#Convertir la colonne Date.de.mesure à une format datetime
Data$Date.de.mesure = as.POSIXct(Data$Date.de.mesure, format="%m/%d/%Y %H:%M")
View(Data)
#Filter data for the first three months of 2014
Data_filtred = dplyr::filter(Data, Date.de.mesure >= as.POSIXct("2014-01-01 00:00") & Date.de.mesure <= as.POSIXct("2014-03-31 23:59"))
View(Data_filtred)
dim(Data_filtred)

###Pré-traitement des données
##Création de nouvelles variables
Data_filtred$Mois = ifelse(month(Data_filtred$Date.de.mesure) == 1, "Janvier",
                             ifelse(month(Data_filtred$Date.de.mesure) == 2, "Février",
                                    ifelse(month(Data_filtred$Date.de.mesure) == 3, "Mars", NA)))
View(Data_filtred)

##Valeurs aberrantes
#Conversion en valeurs numériques
Data_filtred$O. <- as.numeric(Data_filtred$O.)
Data_filtred$WD <- as.numeric(Data_filtred$WD)
Data_filtred$WS <- as.numeric(Data_filtred$WS)
Data_filtred$TEM <- as.numeric(Data_filtred$TEM)
Data_filtred$HUM <- as.numeric(Data_filtred$HUM)
#Visualisation
par(mfrow = c(3, 2))
boxplot(Data_filtred$O.)$out
boxplot(Data_filtred$WD)$out
boxplot(Data_filtred$WS)$out
boxplot(Data_filtred$TEM)$out
boxplot(Data_filtred$HUM)$out
#Verifier l'emplacement
l1 = which(Data_filtred$O. %in% boxplot(Data_filtred$O.)$out)
l2 = which(Data_filtred$WS %in% boxplot(Data_filtred$WS)$out)
l3 = which(Data_filtred$TEM %in% boxplot(Data_filtred$TEM)$out)

sum(is.na(Data_filtred))
#Transformation des valeurs aberrantes en valeurs manquantes
Data_filtred$O.[l1] = NA
Data_filtred$WS[l2] = NA
Data_filtred$TEM[l3] = NA
sum(is.na(Data_filtred)) 

#print(Data_filtred)

##Valeurs manquantes
#Emplacement de données manquantes 
which(is.na(Data_filtred))
#Graphique synthétique de données manquantes
gg_miss_var(Data_filtred)
#Le nombre des valeurs manquantes
sum(is.na(Data_filtred))
#Pourcentage de données manquantes 
sum(is.na(Data_filtred))*100/prod(dim(Data_filtred))
#Observer la distribution de la variable contenant des valeurs manquantes
hist(Data_filtred$O., main = "Histogram d' O3", xlab = "O3 Concentration") #normale
#les données suivent une distribution normale, l'imputation par la moyenne peut être une option simple et raisonnable.
Data_filtred$O.= impute (Data_filtred$O,fun=mean)

hist(Data_filtred$WS, main = "Histogram de WS ", xlab = "La vitesse du vent") # n'est pas parfaitement normale
# On utilise l'imputaion par médiane puisque elle est moins sensible aux valeurs extrêmes que la moyenne.
Data_filtred$WS=impute(Data_filtred$WS, fun=median)


hist(Data_filtred$WD, main = "Histogram de WD ", xlab = "La direction du vent")#asymetrique
Data_filtred$WD=impute(Data_filtred$WD, fun=median)

hist(Data_filtred$TEM, main = "Histogram de TEM ", xlab = "La temperature") # normale
Data_filtred$TEM=impute(Data_filtred$TEM, fun=mean)

hist(Data_filtred$HUM, main = "Histogram de HUM ", xlab = "L'humidité") # n'est pas parfaitement normale
#on a une quantité suffisante de données, on peut aussi envisager l'imputation par les voisins les plus proches
Data_filtred = kNN(Data_filtred, variable = ("HUM"), imp_var = FALSE, k=5)

gg_miss_var(Data_filtred)

##Normalisation des données
#il faut appliquer la mise àl'echelle puisque les variables présentent des échelles différente, certaines variables peuvent dominer l'analyse simplement en raison de leurs valeurs plus grandes
#les données suivent une distribution normale donc on applique la normalisation (plage des valeurs entre 0 et 1 )
print(min(Data_filtred$O.))
Data_filtred$WD = (Data_filtred$WD - min(Data_filtred$WD)) / (max(Data_filtred$WD) - min(Data_filtred$WD))
Data_filtred$WS = (Data_filtred$WS - min(Data_filtred$WS)) / (max(Data_filtred$WS) - min(Data_filtred$WS))
Data_filtred$TEM = (Data_filtred$TEM - min(Data_filtred$TEM)) / (max(Data_filtred$TEM) - min(Data_filtred$TEM))
Data_filtred$HUM = (Data_filtred$HUM - min(Data_filtred$HUM)) / (max(Data_filtred$HUM) - min(Data_filtred$HUM))
View(Data_filtred)


##Encoding
# One-hot encode 'region'
#encoded_region <- model.matrix(~ 0 + region, data = Data_filtred)
#Data_encoded <- cbind(Data_filtred, encoded_region)
#Data_encoded <- Data_encoded[, !grepl("^region$", names(Data_encoded))]

# Convert 'Mois' to numerical using ordinal mapping
month_mapping <- c("Janvier" = 1, "Février" = 2, "Mars" = 3)
Data_encoded$Mois <- as.numeric(factor(Data_encoded$Mois, levels = names(month_mapping)))

View(Data_encoded1)

##Analyse Univariée

summary(Data_filtred)

Data_encoded = Data_filtred

kruskal.test(Data_encoded$O. ~ Data_encoded$region, data = Data_encoded)
# Perform ANOVA
anova_result <- aov(Data_encoded$O. ~ Data_encoded$region, data = Data_encoded)
summary(anova_result)
        
#Distribution des données
#On roduit un graphique QQ normal des variableset on trace la droite de Henry:
par(mfrow = c(3, 2))
qqnorm(Data_encoded$O., datax=TRUE, main="O3")
qqline(Data_encoded$O.,datax=TRUE)
qqnorm(Data_encoded$WD, datax=TRUE, main="WD")
qqline(Data_encoded$WD,datax=TRUE)
qqnorm(Data_encoded$WS, datax=TRUE, main="WS")
qqline(Data_encoded$WS,datax=TRUE)
qqnorm(Data_encoded$TEM, datax=TRUE, main="TEM")
qqline(Data_encoded$TEM,datax=TRUE)
qqnorm(Data_encoded$HUM, datax=TRUE, main="HUM")
qqline(Data_encoded$HUM,datax=TRUE)
#Shapiro-Wilk
set.seed(123) # Setting a seed for reproducibility
sampled_data1 <- sample(Data_encoded$O., 5000)
print(shapiro.test(sampled_data1))
sampled_data2 <- sample(Data_encoded$WD, 5000)
print(shapiro.test(sampled_data2))
sampled_data3 <- sample(Data_encoded$WS, 5000)
print(shapiro.test(sampled_data3))
sampled_data4 <- sample(Data_encoded$TEM, 5000)
print(shapiro.test(sampled_data4))
sampled_data5 <- sample(Data_encoded$HUM, 5000)
print(shapiro.test(sampled_data5))
#Selon les resultats du test de shapiro toutes les variables ne suivent pas une distribution normale mais graphiquement sauf WD toutes les autres variables suivent une distribution normale
#Pour de grands échantillons, même de petites déviations de la normalité peuvent être détectées comme significatives par le test de shapiro d'ou on va prendre en consideration les resultats graphiques

############### Analyse bivariée ################
numeric_data <- Data_encoded[, sapply(Data_encoded, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

numeric_features <- c("O.", "WD", "WS", "TEM", "HUM",'Mois','regionJerba','regionNabeul','regionTozeur')
cor_with_o <- correlation_matrix[,"O."]
print("Pearson's Correlation Coefficients:")
for (i in seq_along(numeric_features)) {
  variable <- names(cor_with_o)[i]
  correlation_coefficient <- cor_with_o[i]
  
  cat("Correlation between 'O3' and", variable, ":", round(correlation_coefficient, 3))
  
  # Interpretation
  if (correlation_coefficient > 0) {
    cat(" (Positive correlation)")
  } else if (correlation_coefficient < 0) {
    cat(" (Negative correlation)")
  } else {
    cat(" (No correlation)")
  }
  cat("\n")
}

corrplot(correlation_matrix, method = "color")

  correlation_spearman <- cor.test(Data_encoded$O.,Data_encoded$WD, method = "spearman")
  print(correlation_spearman)
  

# Créer le nuage de points
par(mfrow = c(2, 2))
plot(Data_encoded$O., Data_encoded$WD, main="Nuage de points O3 et direction de vent",
     xlab="O3", ylab="WD", pch=19, col="blue")
plot(Data_encoded$O., Data_encoded$WS, main="Nuage de points O3 et vitesse de vent",
     xlab="O3", ylab="WS", pch=19, col="red")
plot(Data_encoded$O., Data_encoded$TEM, main="Nuage de points O3 et Temprature",
     xlab="O3", ylab="TEM", pch=19, col="green")
plot(Data_encoded$O., Data_encoded$HUM, main="Nuage de points O3 et Humidité",
     xlab="O3", ylab="HUM", pch=19, col="pink")

plot(Data_encoded$O., Data_encoded$Region, main="Nuage de points O3 et Region",
     xlab="O3", ylab="Region", pch=19, col="purple")
#selon les nuages de points il n'y a aucune relation entre les variables (WD,WS,TEM,HUM) et 03
#Corrélation entre les variables
#cor(Data_filtred$O.,Data_filtred$HUM, method = "pearson")3 normalité
#cor(Data_filtred$O.,Data_filtred$HUM, method = "spearman") #non normalité


plot(Data_encoded$O., Data_encoded$Region, main="Nuage de points O3 et Region",
     xlab="O3", ylab="Region", pch=19, col="purple")

# Test de corrélation de Pearson pour chaque paire de variables
cor_test_O3_WD <- cor.test(Data_encoded$O., Data_encoded$WD)
cor_test_o3_WS <- cor.test(Data_encoded$O., Data_encoded$WS)
cor_test_o3_TEM <- cor.test(Data_encoded$O., Data_encoded$TEM)
cor_test_o3_HUM <- cor.test(Data_encoded$O., Data_encoded$HUM)
# Afficher le résultat du test
print(cor_test_O3_WD)
print(cor_test_o3_WS)
print(cor_test_o3_TEM)
print(cor_test_o3_HUM)

##Régression linéaire
modele1 = lm(formula =  Data_encoded$O. ~ Data_encoded$TEM, data = Data_encoded)
summary(modele1)
#Interpretation : Significativite globale : p − value :< 2.2e − 16 On rejetteH0 ⇒ il existe au moins un parametre non nul .
#Qualite de regression : R2 = 0.03963 ≈ R2 ajusté: la qualite du modele n'est pas trés bonne
#Dans ce cas, environ 3,96 % de la variance de O3 est expliquée par TEM

modele2 = lm(formula =  Data_encoded$O. ~ .- regionNabeul, data = Data_encoded)
summary(modele2)



#Interpretation :
#Qualité de regression : R2 = 0.26 ≈ R2 ajusté
#Dans ce cas, environ 26 % de la variance de O3 est expliquée par ces variables.

#Pour améliorer la performance du modele a chaque fois enlever la variable la moins significative et interpreter la qualite de l'ajustement.
#Les variables moins significatives sont les variables avec la p-value la plus importantes.
# Retirer la variable "HUM"
modele3 = update(modele2, . ~ . - HUM)
summary(modele3)
#Les deux modèles semblent être assez similaires en termes de performance.
#Pas de changement remarquable au niveau de R2 = 0.2599 : la qualite du modele n’est pas affect́ee
#La différence dans le R-squared ajusté est minime, et le F-statistic du nouveau modèle est légèrement plus élevé.

#Pour comparer les statistiques de performance des differents modèles. Plusieurs mesures peuvent être prises en compte,
#telles que le R carré ajusté, l'erreur standard des résidus, et le test F-statistic.
#Le R carré ajusté mesure la proportion de la variance totale de la variable dépendante qui est expliquée par le modèle
#Residual Standard Error mesure de la dispersion des résidus, qui sont les différences entre les valeurs observées et les valeurs prédites par le modèle
#Un F-statistic élevé (comme dans ce cas) indique que le modèle dans son ensemble est significatif. 
#Cependant, cela ne garantit pas que chaque coefficient individuel est significatif. 

#Pour comparer entre deux modeles tels que la qualité d’ajustement est la meme, on utilise l’AIC.
#Le modele avec l’AIC le plus faible est le modele le plus bon.
AIC(modele2)
AIC(modele3)
#Dans ce cas, le modèle avec l'AIC le plus bas est modele3 (-44395).
#Cela suggère que, selon le critère AIC, le modele3 est préférable au modele2.


#Reduction de dimension
# Sélectionner les colonnes pertinentes pour l'ACP (exclure la variable cible)
data_acp <- Data_encoded[, -which(names(Data_encoded) == "O.")]
numeric_columns <- data_acp[, sapply(data_acp, is.numeric)]
View(numeric_columns)
# Effectuer l'ACP
acp_result <- PCA(numeric_columns, graph = TRUE)
#les deux premières composantes principales expliquent 48.17% de la variance totale des données. 


#Cela signifie que ces deux composantes principales capturent l'essentiel de l'information contenue dans les trois variables explicatives initiales.
#Régresser la variable cible sur les composantes principales
modele4 <- lm( Data_encoded$O. ~ acp_result$ind$coord[, 1:2], data =  Data_encoded)
# Afficher le résumé du modèle de régression
summary(modele4)
# le modèle de régression linéaire avec les deux composantes principales (Dim.1 et Dim.2) en tant que variables explicatives
#est statistiquement significatif. Le F-statistique est très élevé (574,1)
#et la valeur p est très petite (< 2,2e-16), ce qui signifie que le modèle s'ajuste bien aux données.

##Modèles additifs généralisés (GAM)
#les GAM essayent d'ajuster au meilleur une fonction de lissage non-linéaire à travers les données, mais tout en contrôlant
#le degré de courbure de la ligne
#le degre de lissage de f(x) qui est contrôlée en utilisant une régression pénalisée qui est déterminée automatiquement selon 
#la méthode d'ajustement
# Création du modèle linéaire
linear_model <- gam(Data_encoded$O. ~ Data_encoded$HUM, data = Data_encoded)
# Création du modèle lisse
smooth_model <- gam(Data_encoded$O. ~ s(Data_encoded$HUM), data = Data_encoded)
#visualiser la non-linéarité du modèle.
plot(smooth_model)
AIC(linear_model, smooth_model)
#AIC du GAM lissé est plus bas, ce qui indique que l'ajout d'une fonction de lissage améliore la performance du modèle.
#La linéarité n'est donc pas soutenue par nos données.
smooth_model


categorical_variables <- c("Region", "Mois")

par(mfrow = c(1, 2))

for (variable in categorical_variables) {
  # Diagramme à barres
  barplot(table(Data_encoded[[variable]]),
          main = paste("Diagramme à barres de", variable),
          xlab = variable,
          col = "skyblue",                                                           
          ylim = c(0, max(table(Data_encoded[[variable]])) + 50))
}
######tableau de fréquence
for (variable in categorical_variables) {
  # Tableau de fréquence
  print(table(Data_encoded[[variable]]))
}
