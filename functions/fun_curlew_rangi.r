


f_plot_shp <- function(shp,land=NULL,xy_lim=NULL) {
    require(ggplot2)
    if(is.null(xy_lim)) {
        bbox <- st_bbox(shp)
        xy_lim <- data.frame(X=c(bbox$xmin,bbox$xmax),Y=c(bbox$ymin,bbox$ymax))
        }

    gg <- ggplot() + theme_bw()
    if(!(is.null(land))) gg <- gg + geom_sf(data =land,fill="white", colour="black", size=0.2, alpha=.5)
    gg <- gg + geom_sf(data =shp,fill="green", colour=NA, size=0.2, alpha=.5)
    gg <- gg + coord_sf(xlim = xy_lim$X, ylim = xy_lim$Y)
    print(gg)
}


