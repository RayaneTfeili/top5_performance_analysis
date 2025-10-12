library(dplyr) ## est utilisé afin de créé une nouvelle variable (catégorie d'age)
library(factoextra)
library(ggplot2)
library(robustbase)
library(robust)
library(rrcov)

##### lecture des donnée + études priliminaire 
data = read.csv("top5-players.csv")

str(data)
summary(data)

#Rajout des année de naissances + âge + nationalité 
data$Born[data$Player == "Marco Pellegrino"] <- 2002
data$Born[data$Player == "Max Moerstedt"] <- 2006
data$Born[data$Player == "Max Svensson"] <- 2001

data$Age[data$Player == "Marco Pellegrino"] <- 21
data$Age[data$Player == "Max Moerstedt"] <- 17
data$Age[data$Player =="Max Svensson"] <- 21

data$Nation[data$Player == "Marco Pellegrino"] <- "ar ARG"
data$Nation[data$Player == "Mahmut Kücüksahin"] <- "tr TUR"



## création d'une nouvelle variable contenant les catégorie d'age 

data <- data %>%
  mutate(cat_age = case_when(
    Age <= 21 ~ "jeune",
    Age >= 22 & Age <= 26 ~ "moyen",
    Age >= 27 & Age <= 31 ~ "expérimenté",
    Age >= 32 ~ "agé"
  ))

data = na.omit(data)

colonne_pca = c("Born","MP", "Starts", "Min","Gls", "Ast", "PK", "PKatt" , "CrdY" , "CrdR", "xG" , "npxG","xAG" , "PrgC", "PrgP" , "PrgR")
data_pca=na.omit(data[,colonne_pca])


### détection outliers
library(MASS)
library(dplyr)
library(robustbase)

data <- read.csv("/home/rt/Desktop/top5-players.csv")
#Rajout des année de naissances + âge + nationalité 
data$Born[data$Player == "Marco Pellegrino"] <- 2002
data$Born[data$Player == "Max Moerstedt"] <- 2006
data$Born[data$Player == "Max Svensson"] <- 2001

data$Age[data$Player == "Marco Pellegrino"] <- 21
data$Age[data$Player == "Max Moerstedt"] <- 17
data$Age[data$Player =="Max Svensson"] <- 21

data$Nation[data$Player == "Marco Pellegrino"] <- "ar ARG"
data$Nation[data$Player == "Mahmut Kücüksahin"] <- "tr TUR"
str(data)

data <- data %>%
  mutate(cat_age = case_when(
    Age <= 21 ~ "jeune",
    Age >= 22 & Age <= 26 ~ "moyen",
    Age >= 27 & Age <= 31 ~ "expérimenté",
    Age >= 32 ~ "agé"
  ))

data = na.omit(data) # retire notre joueur qui pose problème avec bcp de NA

#On considère uniquement les attaquants et les milieux ayant joués au moins 90 minutes sur la saison
data_clean <- data[data$Min >= 90 & data$Pos %in% c("FW", "MF","MF,FW","FW,MF"), ]
#on voit qu’il y a un écart signifcatif entre la moyenne et la médiane pour les variables Gls et Ast
summary(data_clean)

# Boxplot des buts
boxplot(Gls ~ cat_age, data = data_clean,
        col = "skyblue", main = "Buts par tranche d'âge (FW & MF)",
        xlab = "Tranche d'âge", ylab = "Buts ")


# Boxplot des passes décisives
boxplot(Ast ~ cat_age, data = data_clean,
        col = "lightgreen", main = "Passes décisives /par tranche d'âge (FW & MF)",
        xlab = "Tranche d'âge", ylab = "Passes décisives ")

#on voit qu’il y a des outliers, cependant ce n’est pas suffisant car comme nous sommes dans un cas mutlivariées les variables intéragissent entre-elles ainsi nous allons utiliser une méthode de détection multidimensionnelle des valeurs aberrantes 
data_numerique <- data_clean[, c("Gls", "Ast","Age")] 

MCD <- covRob(data_numerique, estim="mcd")

distrob=sqrt(mahalanobis(data_numerique, MCD$center, MCD$cov))
cutoff=sqrt(qchisq(0.975,df=ncol(data_numerique)))
plot(distances, pch = 16, col = ifelse(distances > cutoff,  "red", "blue"),main = "Distances de Mahalanobis Robustes"
)

abline(h=cutoff,col = 'black')


### detection outliers data_pca 

MCD <- covRob(data_pca, estim="mcd")

distrob=sqrt(mahalanobis(data_pca, MCD$center, MCD$cov))
cutoff=sqrt(qchisq(0.975,df=ncol(data_pca)))
plot(distances, pch = 16, col = ifelse(distances > cutoff,  "red", "blue"),main = "Distances de Mahalanobis Robustes"
)

abline(h=cutoff,col = 'black')

#ANALYSES PRELIMINAIRES
#Analyse des statistiques descriptives univariées
summary(data_pca)
boxplot(data_pca) #afin de déceler d'éventuelles valeurs aberrantes

n_joueurs_uniques <- n_distinct(data$Player)
print(n_joueurs_uniques)


########### quel est le lien entre la catégorie d’âge et le championnat ?

## Y-a-t'il des liens entre l'age du joueur et le championnat ( en terme du nombre de joueurs présent)
##ici nous allons faire une ACOBI. 

###création de la table de contingence 
aggregation <- with(data, table(Comp, cat_age))
print(aggregation)

#Test d'indépendance entre le championnat et l'age (hypothèse vérifiée)
test=chisq.test(aggregation)
test ## petite p-valeur donc en rejette que les variables sont indépendente. on peut donc continuer

library(FactoMineR)
bca=CA(aggregation) #ACOBI sur aggregation
bca$eig #valeurs propres, pourcentage d'inertie, pourcentage d'inertie cumulé

#Valeurs propres
bca$eig[,1]
barplot(bca$eig[,1])

#Pourcentage d'inertie expliquée par les axes principaux
bca$eig[,2:3]

#Coordonnées des modalités sur les axes principaux
bca$row$coord #pour les profils lignes
bca$col$coord #pour les profils colonnes

#Contribution des modalités à la construction des axes principaux
bca$row$contrib #pour les profils lignes
bca$col$contrib #pour les profils colonnes

#Qualité de représentation des modalités sur les axes principaux
bca$row$cos2 #pour les profils lignes
bca$col$cos2 #pour les profils colonnes

#### representation barycentrique 

# Coordonnées de "jeune"
coord_jeune <- bca$col$coord["jeune", 1:2]

# Pente du vecteur centre -> "jeune"
dx <- coord_jeune[1]
dy <- coord_jeune[2]

# Calcul de la pente de la perpendiculaire (orthogonale) : -dx/dy
if (dy != 0) {
  perp_slope <- -dx / dy
} else {
  perp_slope <- Inf  # verticale
}

# Création d'un dataframe pour la droite perpendiculaire
t_range <- seq(-1.5, 1.5, length.out = 100)
if (is.finite(perp_slope)) {
  perp_line <- data.frame(
    x = t_range,
    y = perp_slope * t_range
  )
} else {
  # cas spécial si la pente est infinie (droite verticale)
  perp_line <- data.frame(
    x = rep(0, length(t_range)),
    y = t_range
  )
}

# Visualisation
fviz_ca_col(bca, repel = TRUE) +  # Affiche les colonnes = modalités d'âge
  geom_segment(aes(x = 0, y = 0, xend = dx, yend = dy),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red", linetype = "solid", size = 1) +
  geom_line(data = perp_line, aes(x = x, y = y), color = "blue", linetype = "dashed") +
  ggtitle("Vecteur vers 'jeune' et droite perpendiculaire (barycentrique)")



############ Liens entre la présence de jeunes et le temps de jeu des jeunes dans les championnats ?

# Total minutes jouées par tous les joueurs, par championnat
total_minutes <- data %>%
  group_by(Comp) %>%
  summarise(total_min = sum(Min, na.rm = TRUE))

# Minutes jouées par les jeunes

jeune_minutes <- data %>%
  filter(cat_age == "jeune") %>%
  group_by(Comp) %>%
  summarise(jeune_min = sum(Min, na.rm = TRUE))

# Nombre de jeunes joueurs par championnat
jeunes_par_championnat <- data %>%
  filter(cat_age == "jeune") %>%
  group_by(Comp) %>%
  summarise(n_jeunes = n())

# Fusion des 3 tableaux en un seul
recapitulatif <- jeune_minutes %>%
  left_join(total_minutes, by = "Comp") %>%
  left_join(jeunes_par_championnat, by = "Comp") %>%
  mutate(
    prop_min_jeunes = jeune_min / total_min,
    moyenne_jeunes = jeune_min / n_jeunes,
    prop_par_joueur = moyenne_jeunes / total_min
  )



### ACP

### acp robuste
res_robust <- PcaHubert(data_pca, k = 5, scale = TRUE)
summary(res_robust)

#Deuxième étape: déterminer le nombre de composantes principales à conserver
#Règle 1: garder les composantes principales dont les variances sont plus grandes ou égales à 1
eigenvalues=res_robust@eigenvalues
eigenvalues
#on garde comp. 1 et comp.2

#Règle 2: garder les composantes principales qui ont un pourcentage d'inertie élevé
summary(res_robust)
#on garde comp. 1 et comp.2 et comp.3

#Règle 3: règle du coude à l'aide d'un graphique des valeurs propres
plot(eigenvalues, type='line')
points(eigenvalues, pch=20, col='red')
#on garde comp. 1 et comp.2

#Conclusion des trois règles: on garde comp. 1 et comp.2

eigenvectors=res_robust@loadings #valeurs manquantes car R décide de ne pas les afficher
eigenvectors

plot(res_robust@scores[,1:2]) #on projette sur les première et deuxième composantes principales
abline(h=0,v=0)


#Objectif: vérifier que l'interprétation de comp.1 et comp.2 de l'étape 3 soit cohérente
#Des graphiques plus explicites peuvent s'obtenir de la manière suivante:
# représentation des individus en fonction du championnat
# S'assurer que 'Comp' est un facteur
data$Comp <- as.factor(data$Comp)

# Palette de couleurs (5 couleurs pour les 5 ligues)
palette_couleurs <- c("blue", "green3", "red", "magenta", "yellow")

#### on trace 3 graphe comp.1/comp.2 , comp.1/comp.3 et comp.2/comp.3

plot(res_robust@scores[,1],
     res_robust@scores[,2],
     xlab = "Comp. 1",
     ylab = "Comp. 2",
     main = "Representation des individus en fonction du championnat,comp.1/comp.2",
     pch = 19,
     col = palette_couleurs[as.numeric(data$Comp)])
abline(h=0,v=0)
legend("topright",c("Bundesliga","Premier League","La Liga","Ligue 1" , "Serie A"),cex=1,col=c("blue", "green3", "red", "magenta", "yellow"),pch=16)


# représentation des individus en fonction de la catégorie d'age
# S'assurer que 'Comp' est un facteur
data$cat_age <- as.factor(data$cat_age)

# Palette de couleurs (4 couleurs pour les 4 catégorie d'age)
palette_couleurs <- c("blue", "green3", "red", "magenta")

#### on trace 3 graphe comp.1/comp.2 , comp.1/comp.3 et comp.2/comp.3
# Tracer le scatter plot
plot(res_robust@scores[,1],
     res_robust@scores[,2],
     xlab = "Comp. 1",
     ylab = "Comp. 2",
     main = "Representation des individus en fonction de la catégorie d'age,comp.1/comp.2",
     pch = 19,
     col = palette_couleurs[as.numeric(data$cat_age)])
abline(h=0,v=0)
legend("topright",c("agé","expérimenté","jeune","moyen"),cex=1,col=c("blue", "green3", "red", "magenta"),pch=16)

### classement première composante principale

### projeter sur la première composante principale

scores_PC1 <- res_robust@scores[, 1]  # première composante
tableau_PC1 <- data.frame(
  Noms = data$Player,
  score =  scores_PC1 # on prend ici - le score parce que quand on regarde la première composante principale tout les coeff sont négatif 
)
classement_1 <- tableau_PC1[order(-tableau_PC1$score), ] #trie du meilleur score au plus mauvais
head(classement_1)


#Standardiser les données 
data_st=as.data.frame(scale(data_pca))
apply(data_st, 2, mean) #les moyennes sont nulles
apply(data_st, 2, var) #les variances sont égales à 1


#ANALYSE EN COMPOSANTES PRINCIPALES
pca=princomp(data_pca, cor=TRUE) #ACP appliquée sur la matrice de corrélation
summary(pca) #quelques informations sur les composantes principales

#Interpréter les résultats de l'ACP

#Première étape: extraire les valeurs propres de la matrice de corrélation
corr_matrix=cor(data_pca)
eigens=eigen(corr_matrix) 
eigens #valeurs propres et vecteurs propres
eigens$values #valeurs propres de la matrice de corrélation
eigenvalues=pca$sdev^2 #sdev est l'écart-type des composantes principales donc en les élevant au carré, on obtient les valeurs propres

#Deuxième étape: déterminer le nombre de composantes principales à conserver
#Règle 1: garder les composantes principales dont les variances sont plus grandes ou égales à 1
eigenvalues
#on garde comp. 1 et comp.2 et comp.3

#Règle 2: garder les composantes principales qui ont un pourcentage d'inertie élevé
inertie_perc=100*eigenvalues/sum(eigenvalues) #P=somme des valeurs propres
sum(eigenvalues) #vérification
inertie_perc
cumsum(inertie_perc) #pourcentages cumulés
#on garde comp. 1 et comp.2 et comp.3

#Règle 3: règle du coude à l'aide d'un graphique des valeurs propres
plot(eigenvalues, type='line')
points(eigenvalues, pch=20, col='red')
#on garde comp. 1 et comp.2 et comp.3

#Conclusion des trois règles: on garde comp. 1 et comp.2 et comp.3


#Troisième étape: interpréter les composantes principales via les corrélations entre composantes principales et variables aléatoires initiales
#Pour ce faire, il nous faut les vecteurs propres (2 possibilités)
eigens$vectors 

eigenvectors=pca$loadings #valeurs manquantes car R décide de ne pas les afficher
eigenvectors

#Calculs des corrélations entre les deux premières composantes principales et les variables initiales (2 possibilités)
corr1=sqrt(eigenvalues[1])*eigenvectors[,1]
corr2=sqrt(eigenvalues[2])*eigenvectors[,2]

cor1=pca$sdev[1]*eigenvectors[,1]
cor2=pca$sdev[2]*eigenvectors[,2]
print(cbind(cor1,cor2)) #on imprime les deux corrélations

#Cercle des corrélations
plot(cor1, cor2, xlim=c(-1,1), ylim=c(-1,1))
abline(h=0,v=0) #on ajoute l'axe x et y
symbols(0, 0, circles=1, inches=F, add=T) #dessine le cerclce unité
identify(cor1, cor2, labels=colnames(data_pca)) #identifie les points du graphique


#Quatrième étape: projeter les individus sur les composantes principales
#Les coordonnées des individus sur les composantes principales se trouvent dans l'objet scores de pca
pca$scores #tous les scores obtenus pour les six composantes principales
data.matrix(data_st)%*%eigens$vectors #utilise la variance avec 1/n et non 1/(n-1)
pca$scores[,1:2]
plot(pca$scores[,1:2]) #on projette sur les première et deuxième composantes principales
abline(h=0,v=0) 
identify(pca$scores[,1:2],labels=Credoc$Identity) #identifier les points du graphique

#Objectif: vérifier que l'interprétation de comp.1 et comp.2 de l'étape 3 soit cohérente
#Des graphiques plus explicites peuvent s'obtenir de la manière suivante:
# représentation des individus en fonction du championnat
# S'assurer que 'Comp' est un facteur
data$Comp <- as.factor(data$Comp)

# Palette de couleurs (5 couleurs pour les 5 ligues)
palette_couleurs <- c("blue", "green3", "red", "magenta", "yellow")

#### on trace 3 graphe comp.1/comp.2 , comp.1/comp.3 et comp.2/comp.3

plot(pca$scores[,1],
     pca$scores[,2],
     xlab = "Comp. 1",
     ylab = "Comp. 2",
     main = "Representation des individus en fonction du championnat,comp.1/comp.2",
     pch = 19,
     col = palette_couleurs[as.numeric(data$Comp)])
abline(h=0,v=0)
legend("topright",c("Bundesliga","Premier League","La Liga","Ligue 1" , "Serie A"),cex=1,col=c("blue", "green3", "red", "magenta", "yellow"),pch=16)

plot(pca$scores[,1],
     pca$scores[,3],
     xlab = "Comp. 1",
     ylab = "Comp. 3",
     main = "Representation des individus en fonction du championnat,comp.1/comp.3",
     pch = 19,
     col = palette_couleurs[as.numeric(data$Comp)])
abline(h=0,v=0)
legend("bottomright",c("Bundesliga","Premier League","La Liga","Ligue 1" , "Serie A"),cex=1,col=c("blue", "green3", "red", "magenta", "yellow"),pch=16)

plot(pca$scores[,2],
     pca$scores[,3],
     xlab = "Comp. 2",
     ylab = "Comp. 3",
     main = "Representation des individus en fonction du championnat,comp.2/comp.3",
     pch = 19,
     col = palette_couleurs[as.numeric(data$Comp)])
abline(h=0,v=0)
legend("topright",c("Bundesliga","Premier League","La Liga","Ligue 1" , "Serie A"),cex=1,col=c("blue", "green3", "red", "magenta", "yellow"),pch=16)


# représentation des individus en fonction de la catégorie d'age
# S'assurer que 'Comp' est un facteur
data$cat_age <- as.factor(data$cat_age)

# Palette de couleurs (4 couleurs pour les 4 catégorie d'age)
palette_couleurs <- c("blue", "green3", "red", "magenta")

#### on trace 3 graphe comp.1/comp.2 , comp.1/comp.3 et comp.2/comp.3
# Tracer le scatter plot
plot(pca$scores[,1],
     pca$scores[,2],
     xlab = "Comp. 1",
     ylab = "Comp. 2",
     main = "Representation des individus en fonction de la catégorie d'age,comp.1/comp.2",
     pch = 19,
     col = palette_couleurs[as.numeric(data$cat_age)])
abline(h=0,v=0)
legend("topright",c("agé","expérimenté","jeune","moyen"),cex=1,col=c("blue", "green3", "red", "magenta"),pch=16)

plot(pca$scores[,1],
     pca$scores[,3],
     xlab = "Comp. 1",
     ylab = "Comp. 3",
     main = "Representation des individus en fonction de la catégorie d'age,comp.1/comp.3",
     pch = 19,
     col = palette_couleurs[as.numeric(data$cat_age)])
abline(h=0,v=0)
legend("bottomright",c("agé","expérimenté","jeune","moyen"),cex=1,col=c("blue", "green3", "red", "magenta"),pch=16)

plot(pca$scores[,2],
     pca$scores[,3],
     xlab = "Comp. 2",
     ylab = "Comp. 3",
     main = "Representation des individus en fonction de la catégorie d'age,comp.2/comp.3",
     pch = 19,
     col = palette_couleurs[as.numeric(data$cat_age)])
abline(h=0,v=0)
legend("topright",c("agé","expérimenté","jeune","moyen"),cex=1,col=c("blue", "green3", "red", "magenta"),pch=16)


# représentation des individus en fonction de la nation 
# 1. S'assurer que Nation est un facteur
data$Nation <- as.factor(data$Nation)

# 2. Identifier les 10 nations les plus présentes
top10_nations <- names(sort(table(data$Nation), decreasing = TRUE))[1:10]

# 3. Créer une nouvelle variable indiquant si la nation est dans le top 10
data$Nation_top10 <- ifelse(data$Nation %in% top10_nations, as.character(data$Nation), "Autres")
data$Nation_top10 <- factor(data$Nation_top10)

# 4. Créer une palette de couleurs (10 pour les pays + 1 pour "Autres")
palette_couleurs <- c("blue", "green3", "red", "magenta", "yellow",
                            "orange", "cyan", "darkgreen", "purple", "brown", "grey")
                            
# 5. Tracer le plot PCA avec les couleurs par pays
plot(pca$scores[,1],
     pca$scores[,2],
     xlab = "Comp. 1",
     ylab = "Comp. 2",
     main = "Représentation des joueurs selon les 10 Nations les plus présentes",
     pch = 19,
     col = palette_couleurs[as.numeric(data$Nation_top10)])

abline(h=0,v=0)
# 6. Ajouter une légende
legend("topright",
       legend = levels(data$Nation_top10),
       col = palette_couleurs,
       pch = 19)


#Fin des quatre grandes étapes de l'ACP


### classement première composante principale

### projeter sur la première composante principale

scores_PC1 <- pca$scores[, 1]  # première composante
tableau_PC1 <- data.frame(
  Noms = data$Player,
  score = - scores_PC1 # on prend ici - le score parce que quand on regarde la première composante principale tout les coeff sont négatif 
)
classement_1 <- tableau_PC1[order(-tableau_PC1$score), ] #trie du meilleur score au plus mauvais
head(classement_1)

### moyenne pondérer sur les 3 première composante
somme = sum(as.numeric(eigenvalues["Comp.1"]) + as.numeric(eigenvalues["Comp.2"]) + as.numeric(eigenvalues["Comp.3"]))
tableau_classement_2 <- data.frame(
    Noms = data$Player,
    score = - as.numeric(eigenvalues["Comp.1"])/somme * pca$scores[, 1] + as.numeric(eigenvalues["Comp.2"])/somme * pca$scores[, 2] +  as.numeric(eigenvalues["Comp.3"])/somme * pca$scores[, 3]
)
classement_2 <- tableau_classement_2[order(-tableau_classement_2$score), ] #trie du meilleur score au plus mauvais
head(classement_2)


### clustering robuste 

within <- NULL
for (i in 1:11) {
  within[i] <- sum(kmeans(res_robust@scores, centers = i, nstart = 10)$withinss)
}

plot(1:11, within, type = "b", pch = 19,
     xlab = "Nombre de clusters",
     ylab = "Intra-cluster variance (withinss)",
     main = "Méthode du coude")
#(pour moi : graphique silhouette)
#On voit ici qu'à partir de 4 clusters la variance diminue plus lentement
#On va donc choisir 4 clusters mais libre à nous d'en prendre plus

clusters_kmeans=kmeans(res_robust@scores, centers=4)
plot(res_robust@scores, col=clusters_kmeans$cluster, pch=1, cex=1)
legend("topright", legend = paste("Cluster", 1:4), 
       col = 1:4, pch = 1, title = "Clusters", bty = "n", cex = 0.8)

#Résultat très logique, maintenant essayons de voir un peu si nos variables
#de questions de recherches ont des corrélations avec les 4 groupes formés


pca_individus <- complete.cases(data[, colonne_pca])
data_cluster <- data[pca_individus, ]
table(data_cluster$cat_age, clusters_kmeans$cluster)

#testons l'hypothèse nulle comme quoi la catégorie d'âge est indépendante
#des clusters

chisq_test <- chisq.test(table(data_cluster$cat_age, clusters_kmeans$cluster))
chisq_test

#Clairement pas d'indépendance ! Voyons un peu la répartition pour que ce soit
#plus visuel
ggplot(data_cluster, aes(x = factor(clusters_kmeans$cluster), fill = cat_age)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  xlab("Cluster") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Répartition des tranches d'âge dans chaque cluster")

#on peut refaire la même chose avec le championnat 

table(data_cluster$Comp, clusters_kmeans$cluster)

chisq_test <- chisq.test(table(data_cluster$Comp, clusters_kmeans$cluster))
chisq_test

ggplot(data_cluster, aes(x = factor(clusters_kmeans$cluster), fill = Comp)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  xlab("Cluster") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Répartition des championnats dans chaque cluster")

#difficile à dire si il y a vraiment des tendances qui ressortent alors que
#avec les catégories d'age il y avait clairement qqch !
