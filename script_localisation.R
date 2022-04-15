
# carte localisation des courlis sur les motus avec couche land
library(ggplot2)
localisation_courlis<-read.table("localisation_courlis/courlis.csv", sep=",", header=T)


library(sf)
land <- st_read("SIG/land.shp")


ggplot(data = localisation_courlis) +  
  ggplot2::geom_sf(data = land) +
  geom_point(aes(x = location_long,  
                 y = location_lat,  
                 color = "red"),  
             alpha = .3) +
  labs(color = "red") +  
  theme_bw() +
  coord_sf(xlim = c(-147, -149),ylim = c(-20,-12), expand = FALSE) +
  theme_bw()
 



# carte localisation des courlis sur les motus avec couche land restreinte à la zone d'étude uniquement (lagon bleu)

library(sf)
land <- st_read("SIG/land_complet.shp")
land <- st_make_valid(land)



ggplot(data = localisation_courlis) +  
  ggplot2::geom_sf(data = land) +
  geom_point(aes(x = location_long,  
                 y = location_lat,  
                 color = "red"),  
             alpha = .3) +
  labs(color = "red") +  
  theme_bw() +
  coord_sf(xlim = c(-120, -180),ylim = c(30,-20), expand = FALSE) +
  theme_bw()
