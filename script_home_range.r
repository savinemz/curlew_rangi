
library(adehabitatHR)


# dsf les données au format sf


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
