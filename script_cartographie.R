

library(ggplot2)

loc_courlis <- read.csv("Courlis_all_daynight/courlis_all_daynight.csv")
loc_courlis <- subset(loc_courlis,location_long < 0)

# Choix des individus que l'on veut etudier en fonction du type de balise

# Balise OrniTrack
#loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C27")
#loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C32")
#loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C33")
#loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C34")
#loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C40")

#Balise ICARUS
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C01")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C03")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C04")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C05")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C06")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C07")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C08")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C09")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C11")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C12")


# Transformation des coordonnees en donnees spatiales + modification de la projection
courlis_sf <- st_as_sf(loc_courlis, coords = c("location_long","location_lat"))
st_crs(courlis_sf) <- 4326
courlis_sf <- st_transform(courlis_sf,crs=3832)

loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C09")
courlis_sf <- subset(courlis_sf, courlis_sf$bird_id != "C09")


library(sf)
land <- st_read("SIG/land.shp")
land <- st_make_valid(land)
land <- st_transform(land,crs=3832)
land <- st_make_valid(land)




## Catographie des zones emmergés : motus ##################################################################################################

rangi <- st_read("SIG/rangi_motu.shp")

### Carte a 100km du lagon bleu##########################################################################################
## data a 100km du lagon bleu
land_wide <- st_intersection(land,st_buffer(st_union(rangi),100000))
courlis_sf_wide <- st_intersection(courlis_sf,st_buffer(st_union(rangi),100000))


gg <- ggplot()
gg <- gg + geom_sf(data = land_wide)
gg <- gg + geom_sf(data = courlis_sf_wide,aes(colour=bird_id))
gg <- gg + theme_bw() + labs(colour = "")
gg
#ggsave("output/fig_courlis_wide.png",gg)



### Carte à 10km du lagon bleu ###############################################################################################
## data a 10 km du lagon bleu
land_close <- st_intersection(land,st_buffer(st_union(rangi),10000))
courlis_sf_close <- st_intersection(courlis_sf,st_buffer(st_union(rangi),10000))


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
#ggsave("output/fig_courlis_close.png",gg)



### Carte à 1km du lagon bleu #################################################################################################
## data a 1 km du lagon bleu
land_lb <- st_intersection(land,st_buffer(st_union(rangi),1000))
courlis_sf_lb <- st_intersection(courlis_sf,st_buffer(st_union(rangi),1000))

#carte à 1km du Lagon Bleu localisations courlis
gg <- ggplot()
gg <- gg + geom_sf(data = land_lb)
gg <- gg + geom_sf(data = courlis_sf_lb,aes(colour=bird_id),size=1)
gg <- gg + theme_bw() + labs(colour = "",fill="")
gg <- gg + theme(legend.position = 'none')
gg
#ggsave("Rplot/OrniTrack/fig_courlis_lagon_bleu.png",gg)


#carte à 1km du Lagon Bleu habitats
gg <- ggplot()
gg <- gg + geom_sf(data = land_lb)
gg <- gg + geom_sf(data = rangi,aes(fill=habitat),colour=NA,alpha=.7)
gg <- gg + scale_fill_manual(values=vec_fill)
gg <- gg + theme_bw() + labs(colour = "",fill="")
gg <- gg + theme(legend.position = 'bottom')
gg
#ggsave("Rplot/fig_courlis_LB_sansbird3.png",gg, width = 6, height = 8)



### Carte des rats au Lagon Bleu à 1km ####################################################################################
rangi$rat_absence <- rangi$rat !=1

gg <- ggplot()
gg <- gg + geom_sf(data = rangi,aes(fill=rat_absence),colour=NA,alpha=1)
gg <- gg + theme(legend.position = 'none')
gg <- gg + theme_bw()
gg
#ggsave("Rplot/carte_rat.png",gg)










## Catographies des zones emmergés + immergés = atoll #######################################################################################

rangi <- st_read("SIG/rangi_atoll.shp")

### Carte a 100km du lagon bleu##########################################################################################
## data a 100km du lagon bleu
land_wide <- st_intersection(land,st_buffer(st_union(rangi),100000))
courlis_sf_wide <- st_intersection(courlis_sf,st_buffer(st_union(rangi),100000))

gg <- ggplot()
gg <- gg + geom_sf(data = land_wide)
gg <- gg + geom_sf(data = courlis_sf_wide,aes(colour=bird_id))
gg <- gg + theme_bw() + labs(colour = "")
gg
#ggsave("output/fig_courlis_wide.png",gg)



### Carte à 10km du lagon bleu ###############################################################################################
## data a 10 km du lagon bleu
land_close <- st_intersection(land,st_buffer(st_union(rangi),10000))
courlis_sf_close <- st_intersection(courlis_sf,st_buffer(st_union(rangi),10000))

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
#ggsave("output/fig_courlis_close.png",gg)



### Carte à 1km du lagon bleu #################################################################################################
## data a 1 km du lagon bleu
land_lb <- st_intersection(land,st_buffer(st_union(rangi),1000))
courlis_sf_lb <- st_intersection(courlis_sf,st_buffer(st_union(rangi),1000))

#carte à 1km du Lagon Bleu localisations courlis
gg <- ggplot()
gg <- gg + geom_sf(data = land_lb)
gg <- gg + geom_sf(data = courlis_sf_lb,aes(colour=bird_id),size=1)
gg <- gg + theme_bw() + labs(colour = "",fill="")
gg <- gg + theme(legend.position = 'none')
gg
#ggsave("Rplot/fig_courlis_lagon_bleu3.png",gg)


#carte à 1km du Lagon Bleu habitats
rangi_atol <- st_read("SIG/rangi_atoll.shp")
gg <- ggplot()
gg <- gg + geom_sf(data = land_lb)
gg <- gg + geom_sf(data = rangi_atol,aes(fill=habitat),colour=NA,alpha=.7)
gg <- gg + scale_fill_manual(values=vec_fill)
gg <- gg + theme_bw() + labs(colour = "",fill="")
gg <- gg + theme(legend.position = 'bottom')
gg
#ggsave("Rplot/fig_courlis_LB_sansbird3.png",gg, width = 6, height = 8)