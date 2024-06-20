#Importation de la base de données : 
library(readxl)
data_analyse <- read_excel("Projet_analyse_donnees.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))
View(data_analyse)

library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)

#Supression des variables inutiles 

data_analyse$Happiness.Rank<-NULL
data_analyse$Dystopia.Residual<-NULL


#Renomer les variable

names(data_analyse)[3] <-'Happy.Score' #HappinessScore
names(data_analyse)[4] <-'PIB' #GDP.perCapita
names(data_analyse)[6] <-'Expect.Life' #Healt.life.Expectancy
names(data_analyse)[9] <-'Corruption' #ThrustGouvernementCorruption


#Statistiques Descriptives------------------------------------------------------ 

###Map with hapiness Score 

install.packages("ggplot2")
library("ggplot2")
install.packages("rworldmap")
library(rworldmap)


#Statistiques Descriptives 


###Map with hapiness Score 

install.packages("ggplot2")
library("ggplot2")
install.packages("rworldmap")
library(rworldmap)


Map <- data.frame(country=data_analyse$Country,value=data_analyse$Happy.Score)
Value <- joinCountryData2Map(Map, joinCode="NAME", nameJoinColumn="country")
mapCountryData(Value,nameColumnToPlot = "value", mapTitle="Happy Score en fonction du pays", colourPalette=c('yellow','orange','red'))


#boxplot 
summary(data_analyse)

boxplot(data_analyse[,c('Happy.Score', 'PIB', 'Family', 'Expect.Life', 'Freedom', 'Generosity', 'Corruption')],
        col = c("blue"),                 
        main = paste("Boxplot"),     
        ylab = "Quantiles")              


# Matrice de Correlation
cor <- cor(data_analyse[3:9])
corrplot(cor, method = "number")


#Graph bar moyenne du Hapiness Score par region 
data_analyse %>%
  group_by(Region) %>%
  summarise(mean = mean(Happy.Score), n = n())  

a<-ggplot(data=data_analyse, aes(x=Happy.Score, y=Region, fill=Region)) + stat_summary(fun.y = "mean", geom = "bar" ) 
a




#ACP----------------------------------------------------------------------------
# 2 - ACP avec une selection des variables et individus activs et supplementaires
# variables actives : variable de 1 à 12 (les mois) ; les autres variables sont supplementaire (temperature annuelle, latitude, ...)
# individus actifs : individus de 1 à 25 (les capitales) ; les autrse infidivus sont supplementaires (les autres villes) 

install.packages("FactoMineR")
install.packages("factoextra")
library(factoextra)
library(FactoMineR)



#ACP 1 sans variables et ind supp 
res1.pca <- PCA(data_analyse[,4:9])

#ACP 2 
res2.pca <-  PCA(data_analyse,quali.sup=1:2,quanti.sup =3) 


# Histogramme (des valeurs propres) en % de l'inertie):
round(res2.pca$eig,2)
fviz_screeplot (res2.pca, addlabels = TRUE, ylim = c(0, 60))


# Graphiques & tableaux séparés :

# --  1/ Pour Individus -- 
plot.PCA(res2.pca, choix="ind", autoLab = "yes")

#Concaténation des coordonnées, qualité de représentation et contribution pour chaque individu
round(cbind(res2.pca$ind$coord[,1:2],res2.pca$ind$cos2[,1:2],res2.pca$ind$contrib[,1:2]),2)
#Graphique qualité de representation des individus : 
fviz_pca_ind (res2.pca, pointsize = "cos2", pointshape = 22, fill = "pink", repel = TRUE)
plot(res2.pca,  select = "cos2 50", cex=1,  col.ind = "black", title = "Les 50 pays les mieux représentés", cex.main=2, col.main= "darkblue")


# -- 2/ Pour Variables --
plot(res2.pca, choix = "var")
#Concaténation des coordonnées, qualité de représentation et contribution pour chaque variable
round(cbind(res2.pca$var$coord[,1:2],res2.pca$var$cos2[,1:2],res2.pca$var$contrib[,1:2]),2)


# L'ensemble des résultats de l'ACP : 
summary(res2.pca)

# TABLEAUX RESULTATS ACP
# Table coordonnées 
round(res2.pca$var$coord[,1:2],2)
# Table eig + variance + variance cumul
round(res2.pca$eig,2)
# Table dist
round(res2.pca$ind$dist,2)
# Table contribution individus 
round(res2.pca$ind$contrib[,1:2],2)
# Table contribution var 
round(res2.pca$var$contrib[,1:2],2)
# Table cos2 Variable  
round(res2.pca$var$cos2[,1:2],2)
# Table cos2 indivudus 
round(res2.pca$ind$cos2[,1:2],2)





# Classification----------------------------------------------------------------

# Prepare Data
data_analyse <- na.omit(data_analyse) # listwise deletion of missing
data_analyse  <- scale(data_analyse [3:9]) # standardize variables



# Determine number of clusters, nombre de partition possible,nombre de classe optimal
wss <- (nrow(data_analyse)-1)*sum(apply(data_analyse ,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(data_analyse,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="2",
     ylab="2")

# K-Means Cluster Analysis,calculer la meilleure classe possible
fit <- kmeans(data_analyse, 4) # 4 cluster solution
# get cluster means
aggregate(data_analyse,by=list(fit$cluster),FUN=mean)
# append cluster assignment,profif de chaque groupe pour chaque variable
data_analyse <- data.frame(data_analyse, fit$cluster)

# Ward Hierarchical Clustering,calcul la distance entre les individus et le dendogram
d <- dist(data_analyse, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram

groups <- cutree(fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")




