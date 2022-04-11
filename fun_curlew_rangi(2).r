
f_shp_rangi <- function(shp_processing) {

    ## fabrication de la carto a partir des couches numeriser sous QGIS
    ## immerge = tout - ( ocean  + lagon + lagon_bleu + land)
    ## beach = land - (platier + foret_dense + foret_clair + vasiere)
    ## reef = platier -  (foret_dense + foret_clair + vasiere)
    ## foret_clair = foret_clair - (foret_dense - vasiere)




    if(shp_processing) {
        shp_name <-   c("atoll","ocean","blue_lagoon","lagoon","land","reef","bush","forest_dense","forest_clairseme","mudflat")
        vec_shp <- paste0("../GIS/rangi_",shp_name,".shp")
        vec_shp

        l_shp <- list()
        for(i in 1:length(vec_shp)) {
            shp_file <- vec_shp[i]
            shp <- st_read(shp_file)
            shp$habitat <- shp_name[i]
            shp$id <- i
            shp <- shp[,c("id","habitat")]
            shp <- st_transform(shp,crs=3832)
            l_shp[[i]]  <-  shp
            names(l_shp)[i]  <-  shp_name[i]
        }

    vec_habitat <- c("ocean","blue_lagoon","lagoon","immerge_rocks","beach","reef","bush","forest_dense","forest_clairseme","mudflat")

        l_shp[[11]]  <-  st_difference(l_shp$atoll,st_union(st_union(l_shp$ocean,l_shp$blue_lagoon),l_shp$lagoon))
        name(l_shp)[11] <- "immerge_rocks"
        l_shp[[11]]$habitat  <-  "immerge_rocks"
            l_shp[[11]]$ id  <-  11

        l_shp[[12]]  <-  st_difference(l_shp$land,st_union(st_union(st_union(st_union(l_shp$reef,l_shp$bush),l_shp$forest_dense),l_shp$forest_clairseme),l_shp$mudflat))
        name(l_shp)[12] <- "beach"
        l_shp[[12]]$habitat  <-  "beach"
        l_shp[[12]]$ id  <-  12


        l_shp$reef  <-  st_difference(l_shp$reef,st_union(st_union(st_union(l_shp$bush,l_shp$forest_dense),l_shp$forest_clairseme),l_shp$mudflat))

        l_shp$clairseme  <-  st_difference(l_shp$clairseme,st_union(st_union(l_shp$bush,l_shp$forest_dense),l_shp$mudflat))



        rangi <- l_shp$ocean

        for(h in vec_habitat[-1]) {
            add_shp  <-  l_shp[names(l_shp == h)]
        rangi <- rbind(rangi,add_shp)
    }

    marge <-100
    bbox_reef <- st_bbox(st_buffer(rangi[2,],marge))

    rangi_crop <- st_crop(rangi,bbox_reef)

    st_write(rangi_crop, "../GIS/rangi_crop.shp")
} else {

    rangi_crop <- st_read("../GIS/rangi_crop.shp")
}
  return(rangi_crop)
}
