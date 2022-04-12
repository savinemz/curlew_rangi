library(sf)
library(data.table)
library(dplyr)

## source("functions/fun_curlew_rangi.r")


## f_shp_rangi(TRUE)



    if(shp_processing){
        shp_name <-   c("atoll","blue_lagoon","bush","forest_clairseme","forest_dense","lagoon","land","mudflat","ocean","reef_buffer","rocks")
        vec_shp <- paste0("SIG/",shp_name,".shp")
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

    vec_habitat <-c("atoll","blue_lagoon","bush","forest_clairseme","forest_dense","lagoon","land","mudflat","ocean","reef","rocks")


        l_shp[[11]]  <-  st_difference(l_shp$atoll,st_union(st_union(l_shp$ocean,l_shp$blue_lagoon),l_shp$lagoon))
        names(l_shp)[11] <- "immerge_rocks"
        l_shp[[11]]$habitat  <-  "immerge_rocks"
        l_shp[[11]]$ id  <-  11



        l_shp[[12]]  <-  st_difference(l_shp$land,(st_union(st_union(st_union(st_union(l_shp$reef,l_shp$bush),l_shp$forest_dense),l_shp$forest_clairseme),l_shp$mudflat)))
        names(l_shp)[12] <- "beach"
        l_shp[[12]]$habitat  <-  "beach"
        l_shp[[12]]$ id  <-  12

# 6911851.9886720898 -1695661.1188752274

        l_shp$reef  <-  st_difference(l_shp$reef,st_union(st_union(st_union(l_shp$bush,l_shp$forest_dense),l_shp$forest_clairseme),l_shp$mudflat))

        l_shp$clairseme  <-  st_difference(l_shp$clairseme,st_union(st_union(l_shp$bush,l_shp$forest_dense),l_shp$mudflat))

        l_shp$bush  <-  st_difference(l_shp$bush,st_union(st_union(st_union(l_shp$reef,l_shp$forest_dense),l_shp$clairseme),l_shp$rocks))



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
