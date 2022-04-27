library(sf); library(dplyr)

#calculs des surfaces par polygones: methode 1
rangi <- st_read("SIG/rangi.shp")
rangi$area_poly <- st_area(rangi)



#calcul des surfaces total par habitat
#sum(rangi$area [rangi$habitat == "beach"])
#sum(rangi$area [rangi$habitat == "bush"])
#sum(rangi$area [rangi$habitat == "dense_forest"])
#sum(rangi$area [rangi$habitat == "sparse_forest"])
#sum(rangi$area [rangi$habitat == "mudflat"])
#sum(rangi$area [rangi$habitat == "reef"])
#sum(rangi$area [rangi$habitat == "rocks"])
#sum(rangi$area [rangi$habitat == "shallow"])
#sum(rangi$area [rangi$habitat == "blue_lagoon"])
#sum(rangi$area [rangi$habitat == "lagoon"])
#sum(rangi$area [rangi$habitat == "ocean"])


area_habitat <- aggregate(area_poly~habitat, rangi, sum)


#calcul des surfaces par motu (methode boucle)
#area_motu <- rep(NA,153)  #aires <- numeric(length=153) meme chose que rep() mais en mieux parce que c'est la formule de RL
#A<-NA

#for(i in 1:153){
  #A<- sum(rangi$area_poly [rangi$id_motu == i])

  #area_motu [i]<-A
#}


#calcul des surfaces par motu (methode vecteur)
area_motu <- aggregate(area_poly~id_motu, rangi, sum)
names(area_motu)[2] <- "area_motu"
rangi <- merge(rangi, area_motu, by = "id_motu")

#identifiant des polygones
rangi$id_poly <- 1: nrow(rangi)
rangi <- rangi %>% relocate(id_poly, .after = id_motu)

#proportion des habitats par motu
rangi$proportion <- (rangi$area_poly/rangi$area_motu)
rangi <- rangi %>% relocate(proportion, .after = area_motu)

# localisation des courlis par polygone
loc_courlis <- read.csv("localisation_courlis/courlis.csv")
loc_courlis <- subset(loc_courlis,location_long < 0)

courlis_sf <- st_as_sf(loc_courlis, coords = c("location_long","location_lat"))
st_crs(courlis_sf) <- 4326
courlis_sf <- st_transform(courlis_sf,crs=3832)
sum_loc <- st_intersection(rangi, courlis_sf)


## ajout de la colonne date on pourrait le faire sur les donnees courlis directement ce serait mieux
loc_courlis$date <- substr(loc_courlis$timestamp,1,10)
# est ce que je merge date dans rangi?
sum_loc$date <- substr(sum_loc$timestamp,1,10)






library(data.table)
setDT(sum_loc)
sum_loc <- sum_loc[,.(occurence = .N),by=.(id_poly,bird_id,date)][,.(occurence = .N),by=.(id_poly)]

rangi <- merge(rangi, sum_loc, bx = "id_poly", all.x = T)
rangi$occurence[is.na(rangi$occurence)] <- 0
rangi <- rangi %>% relocate(occurence, .after = habitat)




#création data.table pour d_gg
rangi_DT <- rangi
setDT(rangi_DT)
rangi_DT[,occupation := occurence>0]

#creation d'un tableau a partir de rangi_DT
#calcul prop_mean/habitat/occupation
prop_mean <- aggregate(proportion~habitat + occupation, rangi_DT, mean)
names(prop_mean)[3] <- "prop_mean"

#solution qui marche aussi
#rangi_prop_mean <- aggregate(rangi_DT$proportion, by=list("habitat"=rangi_DT$habitat, "occupation"=rangi_DT$occupation), FUN=mean)


#calcul prop_med/habitat/occupation
prop_med <- aggregate(proportion~habitat + occupation, rangi_DT, median)
names(prop_med)[3] <- "prop_med"


#regroupement des donnees
rangi_DTsm <- merge(prop_mean, prop_med, bx =.(habitat, occupation))

#calcul des quantiles
quantile_inf95 <- aggregate(proportion~habitat + occupation, rangi_DT, function(proportion) quantile (proportion, probs = 0.975))
quantile_sup95 <- aggregate(proportion~habitat + occupation, rangi_DT, function(proportion) quantile (proportion, probs = 0.025))

names(quantile_inf95)[3] <- "inf95"
names(quantile_sup95)[3] <- "sup95"

rangi_DTsm <- merge(rangi_DTsm, quantile_inf95, bx =.(habitat, occupation))
rangi_DTsm <- merge(rangi_DTsm, quantile_sup95, bx =.(habitat, occupation))



## la methode en une ligne de RL
rangi_DT[,proportion := as.numeric(proportion)]
d_gg <- rangi_DT[,.(prop_mean = mean(proportion),prop_med = median(proportion),inf95 = quantile(proportion, 0.025),sup95 = quantile(proportion, 0.975)), by=.(habitat,occupation)]



# graphique = proportion moyenne des habitats en fonction de l'occupation des motus (T, F) par habitat
library(ggplot2); library(units)

#graphique + chgmt couleur par occupation avec intervalles 95%
gg2 <-    ggplot(data = d_gg, aes(x = habitat, y = prop_mean, fill = occupation, group = occupation)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = inf95, ymax = sup95), width = 0.5,alpha=.5,size=1) +
  scale_fill_brewer(palette="Paired") + theme_minimal()
gg2

#graphique RL
gg <- ggplot(data = d_gg, aes(x = habitat, y = prop_mean,fill = occupation,colour=occupation,group=occupation))
gg <- gg + geom_errorbar(aes(ymin = inf95, ymax = sup95),width = 0.5,alpha=.5,size=1)
gg <- gg +  geom_point(alpha=.8,size=2)
gg







#creation tableur pour l'ouverture des donnees numeriques => ACP
library(tidyr)
rangi_PCA <- rangi_DT[,-c(1,4,5,6,8,9)]
rangi_PCA <-pivot_wider(rangi_PCA, names_from = "habitat",
            values_from = "proportion")
rangi_PCA[is.na(rangi_PCA) == T] <- 0
row.names (rangi_PCA) <- rangi_PCA$id_motu
rangi_PCA <- rangi_PCA[,-1]
rangi_PCA <- rangi_PCA[,-c(1,2,3,4)]

# realisation de l'APC avec la librairie FactoMineR
library("FactoMineR")
library("factoextra")
library(ade4)

ACP <- PCA(rangi_PCA, scale.unit = TRUE, ncp = 5, graph = TRUE)
ACP <- print(rangi_PCA)
ACP <- dudi.pca(rangi_PCA, center = TRUE, scale = TRUE, scannf = FALSE)
ACP

# extraction des valeurs propres et la proportion de variances retenues par les composantes principales
eig.val <- get_eigenvalue(ACP)
eig.val
            #Dim1,2,3,4>1 = les composantes principales (PC) concernée représentent 
            #plus de variance par rapport à une seule variable d'origine,lorsque les données sont standardisées.
            # eigenvalue>1 est généralement utilisé comme seuil à partir duquel les PC sont conservés.


# graphique des valeurs propres
plot_eigenvalue <- fviz_eig(ACP, addlabels = TRUE, ylim = c(0, 40))
plot_eigenvalue

#extraction des résultats
var <- get_pca_var(ACP)
var

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)


# Coordonnées des variables
head(var$coord, 4)

# graphique de corrélation des variables
fviz_pca_var(ACP, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Contrib_var",
             geom.ind = "point",
             repel = TRUE
)
            #le graphique montre les relations entre toutes les variables.
            #Les variables positivement corrélées sont regroupées.
            #Les variables négativement corrélées sont positionnées sur les côtés opposés de l'origine du graphique (quadrants opposés).
            #La distance entre les variables et l'origine mesure la qualité de représentation des variables. 
            #Les variables qui sont loin de l'origine sont bien représentées par l'ACP.



fviz_pca_ind(ACP, col.ind="coord", geom = "point", pointsize = 3) +
  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.6, space = "Lab")+ theme_minimal()








#ne marche pas
fviz_pca_ind(ACP,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = "coord", fill.ind = var$coord,
             gradient.cols = c("blue", "yellow", "red"),
             #addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)

#test mais je sais pas si ça peut servir
fviz_pca_biplot(ACP, repel = TRUE,
                geom.ind = "point",
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)


            
head(var$cos2, 4)

#library("corrplot")
#corrplot(var$cos2, is.corr=FALSE)

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(ACP, choice = "var", axes = 1:2)
            #bonne representation de la variable reef
            #moins bonne representation de la variable foret clairsemee


library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# Contributions des variables à PC1
fviz_contrib(ACP, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(ACP, choice = "var", axes = 2, top = 10)
# La contribution totale à PC1 et PC2
fviz_contrib(ACP, choice = "var", axes = 1:2, top = 10)
            #reef et dense_forest ont une contribution supérieure au seuil (ligne en pointillé rouge)
            #ces variables peuvent être considérée comme importantes pour contribuer à la composante?


# visualisation des motus en fonction de cos2
fviz_pca_ind(ACP, col.ind="cos2", geom = "point", pointsize = 3) +
  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.6, space = "Lab")+ theme_minimal()


# visualisation des motus par contribution                   
fviz_pca_ind(ACP, col.ind="contrib", geom = "point", pointsize = 3) +
  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=3, space ="Lab")+ theme_minimal()         





