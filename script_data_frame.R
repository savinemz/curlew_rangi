

#calculs des surfaces par polygones: methode 1
rangi$area <- st_area(rangi)
rangi$area


#calculs des surfaces par polygones: methode 2
library(sf); library(dplyr)
area_polygone <- st_read("SIG/rangi.shp")

area_polygone %>% 
  mutate(area = st_area(area_polygone))


#calcul des surfaces total par habitat
sum(rangi$area [rangi$habitat == "beach"])
sum(rangi$area [rangi$habitat == "bush"])
sum(rangi$area [rangi$habitat == "dense_forest"])
sum(rangi$area [rangi$habitat == "sparse_forest"])
sum(rangi$area [rangi$habitat == "mudflat"])
sum(rangi$area [rangi$habitat == "reef"])
sum(rangi$area [rangi$habitat == "rocks"])
sum(rangi$area [rangi$habitat == "shallow"])
sum(rangi$area [rangi$habitat == "blue_lagoon"])
sum(rangi$area [rangi$habitat == "lagoon"])
sum(rangi$area [rangi$habitat == "ocean"])


#calcul des surfaces par motu


area_motu <- rep(NA,153)
A<-NA

for(i in 1:153){
 A<- sum(rangi$area [rangi$id_motu == i])
  
  aires[i]<-A
}



# Creation data frame
land <- st_read("SIG/land.shp")
rangi <- st_read("SIG/rangi.shp")

id_polygones <- c(land[,1])
habitats <- c(rangi[,1])
surfaces <- c(rangi$area)
id_motus <- c(rangi[,2])


data <- data.frame(id_polygones, habitats, surfaces, id_motus,
                   colnames("id_polygone","habitat", "surface", "id_motu"))



