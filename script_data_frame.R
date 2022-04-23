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

rangi_DTsm <- cbind(rangi_DTsm, quantile_inf95$`inf95`, quantile_sup95$`sup95`) 
#cbind() = meme chose que merge() quand les tableaux sont equivalents

names(rangi_DTsm)[5] <- "inf95"
names(rangi_DTsm)[6] <- "sup95"




#la methode en une ligne de RL mais que je n'arrive pas à faire tourner      
d_gg <- rangi_DT [,.(prop_mean = (mean(proportion)),(prop_med = median(proportion)),(inf95 = quantile(proportion, 0,025)),(sup95 = quantile(proportion, 0,975)), by=.(habitat), by =.(occupation))]
#d_gg <- rangi_DT [,.(prop_mean = (mean(proportion)),(prop_med = median(proportion)),(inf95 = quantile(proportion, 0,025)),(sup95 = quantile(proportion, 0.975)), bx=.(habitat, occupation))]


          
# graphique = proportion moyenne des habitats en fonction de l'occupation des motus (T, F) par habitat
library(ggplot2)

gg <- ggplot(data = rangi_DTsm, (aes (x = habitat, y = prop_mean, fill = habitat, group = occupation))) 
gg <- geom_bar(stat="identity", position = "dodge")
gg <- geom_errorbarh(aes(ymin = inf95, ymax = sup95))
gg <- geom_smooth(data = rangi_DTsm, stat = "smooth", position = "identity")
gg
#ne donne pas de resusltats concret pour le moment = à retravailler lundi





