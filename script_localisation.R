
# carte localisation des courlis sur les motus avec couche land
library(ggplot2)
# localisation_courlis <- read.table("localisation_courlis/courlis.csv", sep=",", header=T)

                                        # [RL] je te conseil des nom d'objet plus court
                                        # il faut qu'ils soient le plus court possible tout en restant comprehenssible
                                        # ici d, data, courlis serai des bon candidats
localisation_courlis <- read.csv("data/courlis.csv")

## il y a une donnes pourri on nettoie
localisation_courlis <- subset(localisation_courlis,location_long < 0)



library(sf)
land <- st_read("SIG/land.shp")
                                        # [RL] land n'est pas propre il faut la forcer a etre valide
land <- st_make_valid(land)

                                        # [RL] on a un difficulte avec nos donnees nous sommes dans le pacifique
                                        # et donc si tu affiche des donnees qui ont des extente large
                                        # tu croises la ligne de changement de jour 180° -180°
                                        # ggplot (mais pas que lui) ne sais pas gérer ca
                                        # si tu n'affiche que Rangiroa tu n'aura pas de probleme
                                        # mais si tu affiche toutes les données courlis et surtout toute la couche land
                                        # alors il faut que tu transforme la projection
                                        # d'une maniere general comme tu va faire des calculs de surface
                                        # il faut que tu reprojette tes données selon une projection qui conserve les distance
                                        # ce n'est pas le cas de WGS84 qui conserve les angles mais pas les distances

                                        # je te conseil la projecion EPSG 3832

land <- st_transform(land,crs=3832)
land <- st_make_valid(land)
                                        # [RL] on transforme aussi les data courlis
courlis_sf <- st_as_sf(localisation_courlis, coords = c("location_long","location_lat"))
st_crs(courlis_sf) <- 4326 # c'est pour WGS84
courlis_sf <- st_transform(courlis_sf,crs=3832)

rangi <- st_read("SIG/rangi.shp")

## data a 100km du lagon bleu
land_wide <- st_intersection(land,st_buffer(st_union(rangi),100000))
courlis_sf_wide <- st_intersection(courlis_sf,st_buffer(st_union(rangi),100000))

## data a 10 km du lagon bleu
land_close <- st_intersection(land,st_buffer(st_union(rangi),10000))
courlis_sf_close <- st_intersection(courlis_sf,st_buffer(st_union(rangi),10000))



# [RL] je te conseil de mettre ta figure dans un objet que tu agrementes de couche au fur et a mesure
gg <- ggplot()
gg <- gg + geom_sf(data = land_wide)
gg <- gg + geom_sf(data = courlis_sf_wide,aes(colour=bird_id))
gg <- gg + theme_bw() + labs(colour = "")
gg
ggsave("output/fig_courlis_wide.png",gg)


tab_fill  <- read.csv("library/colour_habitat.csv")
vec_fill <- tab_fill$colour
names(vec_fill) <- tab_fill$habitat

gg <- ggplot()
gg <- gg + geom_sf(data = land_close)
gg <- gg + geom_sf(data = rangi,aes(fill=habitat),colour=NA,alpha=.5)
gg <- gg + scale_fill_manual(values=vec_fill)
gg <- gg + geom_sf(data = courlis_sf_lb,aes(colour=bird_id),shape =21,fill="white",size=1)
gg <- gg + theme_bw() + labs(colour = "",fill="")
gg
ggsave("output/fig_courlis_close.png",gg)




# carte localisation des courlis sur les motus avec couche land restreinte à la zone d'étude uniquement (lagon bleu)


## data a 1 km du lagon bleu
land_lb <- st_intersection(land,st_buffer(st_union(rangi),1000))
courlis_sf_lb <- st_intersection(courlis_sf,st_buffer(st_union(rangi),1000))



gg <- ggplot()
gg <- gg + geom_sf(data = land_lb)
gg <- gg + geom_sf(data = rangi,aes(fill=habitat),colour=NA,alpha=.7)
gg <- gg + scale_fill_manual(values=vec_fill)
gg <- gg + geom_sf(data = courlis_sf_lb,aes(colour=bird_id),shape =21,fill="white",size=1)
gg <- gg + theme_bw() + labs(colour = "",fill="")
gg
ggsave("output/fig_courlis_lagon_bleu.png",gg)

##
##library(sf)
##land <- st_read("SIG/land_complet.shp")
##land <- st_make_valid(land)
##
##
##
##ggplot(data = localisation_courlis) +
##  ggplot2::geom_sf(data = land) +
##  geom_point(aes(x = location_long,
##                 y = location_lat,
##                 color = "red"),
##             alpha = .3) +
##  labs(color = "red") +
##  theme_bw() +
##  coord_sf(xlim = c(-120, -180),ylim = c(30,-20), expand = FALSE) +
##  theme_bw()
##
