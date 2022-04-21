library(sf); library(dplyr)

#calculs des surfaces par polygones: methode 1
rangi <- st_read("SIG/rangi.shp")
rangi$area_poly <- st_area(rangi)
rangi$area_poly



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
area_habitat


#calcul des surfaces par motu (methode boucle)
area_motu <- rep(NA,153)  #aires <- numeric(length=153) meme chose que rep() mais en mieux parce que c'est la formule de RL
A<-NA

for(i in 1:153){
 A<- sum(rangi$area_poly [rangi$id_motu == i])

  area_motu [i]<-A
}


#calcul des surfaces par motu (methode vecteur)
area_motu <- aggregate(area_poly~id_motu, rangi, sum)
names(area_motu)[2] <- "area_motu"
area_motu
rangi <- merge(rangi, area_motu, by = "id_motu")

#identifiant des polygones
rangi$id_poly <- 1: nrow(rangi)
rangi <- rangi %>% relocate(id_poly, .after = id_motu)


#proportion des habitats par motu
rangi$proportion <- (rangi$area_poly/rangi$area_motu)
                                        # [RL] ATTENTION un proportion c'est entre 0 et 1
                                        # je n'avais pas vu que en faisait un pourcentage et
                                        # je ne comprenais pas les valeurs
                                        # il faut bien nommer les variables pour eviter les confusions
                                        # si proportion poly/motu si pourcentage poly/motu*100

rangi <- rangi %>% relocate(proportion, .after = area_motu)

# localisation des courlis par polygone
loc_courlis <- read.csv("localisation_courlis/courlis.csv")
loc_courlis <- subset(loc_courlis,location_long < 0)

courlis_sf <- st_as_sf(loc_courlis, coords = c("location_long","location_lat"))
                                        # [RL] il faut renseigner le CRS
st_crs(courlis_sf) <- 4326
                                        # [RL] et faire la transformation pour que les donnees courlis soient
                                        # projetees dans le meme referentiel que les polygones de rangi
courlis_sf <- st_transform(courlis_sf,crs=3832)


## st_agr(courlis_sf) = "constant"
## st_agr(rangi) = "constant"
                                        # [RL] je ne sais pas à quoi sert la fonction st_agr
sum_loc <- st_intersection(rangi, courlis_sf)

## sum_loc <- st_intersection(st_geometry(rangi), st_geometry(courlis_sf))


                                        # [RL] a partir de la on peut faire plein de chose
## pour commencer on va garder qu'un info de presence dans un polygon par oiseau et par jour
## pour eviter les double comptage et les problemes de non independence des donnees

## ajout de la colonne date on pourrai le faire sur les donnees courlis directement ce serai mieux
sum_loc$date <- substr(sum_loc$timestamp,1,10)

library(data.table)
setDT(sum_loc)
sum_loc <- sum_loc[,.(occurence = .N),by=.(id_poly,bird_id,date)][,.(occurence = .N),by=.(id_poly)]

rangi <- merge(rangi, sum_loc, bx = "id_poly", all.x = T)
rangi$occurence[is.na(rangi$occurence)] <- 0






# Creation data frame
#land <- st_read("SIG/land.shp")
#rangi <- st_read("SIG/rangi.shp")

#id_polygones <- c(land[,1])
#habitats <- c(rangi[,1])
#surfaces <- c(rangi$area)
#id_motus <- c(rangi[,2])


#data <- data.frame(habitats, surfaces, id_motus,
                   #colnames("habitat", "surface", "id_motu"))




