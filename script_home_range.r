
library(adehabitatHR)
library(sf)

#importation des donnees
loc_courlis <- read.csv("Courlis_all_daynight/courlis_all_daynight.csv")
loc_courlis <- subset(loc_courlis,location_long < 0)


# Transformation des coordonnees en donnees spatiales + modification de la projection
courlis_sf <- st_as_sf(loc_courlis, coords = c("location_long","location_lat"))
st_crs(courlis_sf) <- 4326
courlis_sf <- st_transform(courlis_sf,crs=3832)

#suppression de l'individu C09 (donnees insuffisantes)
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C09")
courlis_sf <- subset(courlis_sf, courlis_sf$bird_id != "C09")



courlis_sf_k <- courlis_sf[,c("bird_id")]
courlis_sf_k <- as(courlis_sf_k,'Spatial')

kdh <- kernelUD(courlis_sf_k, h="href")
## si ne fonctionne pas tester h="href"
image(kdh)





turtle.kernel.poly <- getverticeshr(kdh, percent = 95) 
print(turtle.kernel.poly)
plot(turtle.kernel.poly)

sp::plot(turtle.kernel.poly, col = 1:4)






turtle.kernel.poly <- getverticeshr(kdh[[12]], percent = 95) 
print(turtle.kernel.poly)
plot(turtle.kernel.poly)

sp::plot(turtle.kernel.poly, col = 1:4)



# creating SpatialPolygonsDataFrame
kd_names <- names(kdh)
ud_95 <- lapply(kdh, function(x) try(getverticeshr(x, 50)))


sapply(1:length(ud_95), function(i) {
  row.names(ud_95[[i]]) <<- kd_names[i]
})
sdf_poly_95 <- Reduce(rbind, ud_95)
df_95 <- fortify(sdf_poly_95)
df_95$bird_id <- df_95$id



ud_50 <- lapply(kdh, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50), function(i) {
  row.names(ud_50[[i]]) <<- kd_names[i]
})
sdf_poly_50 <- Reduce(rbind, ud_50)
df_50 <- fortify(sdf_poly_50)
df_50$bird_id <- df_50$id



gg <- ggplot()  + theme_bw()
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95, aes(x = long, y = lat, color = bird_id, group = group),size=1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50, aes(x = long, y = lat, color = bird_id, group = group),size=1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = dsf,aes(group=bird_id,colour= bird_id),size=0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 75% and 95%")
gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
gg <- gg + scale_fill_manual(values=vec_colour)
gg

























dsf_ud  <- dsf[,c("bird_id")]

dsf_ud <- as(dsf_ud,'Spatial')

kdh <- kernelUD(dsf_ud,h="LSCV", grid=500, extent=100)
## si ne fonctionne pas tester h="href"
image(kd)

                                        # creating SpatialPolygonsDataFrame
kd_names <- names(kd)
ud_95 <- lapply(kd, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95), function(i) {
    row.names(ud_95[[i]]) <<- kd_names[i]
})
sdf_poly_95 <- Reduce(rbind, ud_95)
df_95 <- fortify(sdf_poly_95)
df_95$bird_id <- df_95$id



ud_50 <- lapply(kd, function(x) try(getverticeshr(x, 50)))
                                        # changing each polygons id to the species name for rbind call
sapply(1:length(ud_50), function(i) {
    row.names(ud_50[[i]]) <<- kd_names[i]
})
sdf_poly_50 <- Reduce(rbind, ud_50)
df_50 <- fortify(sdf_poly_50)
df_50$bird_id <- df_50$id



gg <- ggplot()  + theme_bw()
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95, aes(x = long, y = lat, color = bird_id, group = group),size=1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50, aes(x = long, y = lat, color = bird_id, group = group),size=1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = dsf,aes(group=bird_id,colour= bird_id),size=0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 75% and 95%")
gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
gg <- gg + scale_fill_manual(values=vec_colour)
gg
