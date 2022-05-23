
library(sf)
library(data.table)
library(dplyr)

                                        # source("functions/fun_curlew_rangi.r")





shp_name <-   c("atoll","blue_lagoon","bush","forest_clairseme_buffer","forest_dense","lagoon","mudflat","ocean","reef_buffer","rocks")
vec_shp <- paste0("SIG/",shp_name,".shp")
vec_shp

vec_habitat <-c("atoll","blue_lagoon","bush","sparse_forest","dense_forest","lagoon","mudflat","ocean","reef","rocks")


l_shp <- list()
for(i in 1:length(vec_shp)) {
    shp_file <- vec_shp[i]
    shp <- st_read(shp_file)
    shp <- st_transform(shp,crs=3832)
    shp <- st_union(shp)
    shp <- st_as_sf(shp)
    shp <- st_make_valid(st_simplify(shp,dTolerance=1))
    shp$habitat <- vec_habitat[i]
    shp <- shp[,c("habitat")]
    l_shp[[i]]  <-  shp
    names(l_shp)[i]  <-  shp_name[i]
}

land <- st_read("SIG/land.shp")
land <- st_transform(land,crs=3832)
land <- st_buffer(land,dist =1)
land <- st_make_valid(land)
land <- st_simplify(land,dTolerance = 1)
land <- st_intersection(land,l_shp$atoll)
land$id_motu <- 1:nrow(land)
land <- land[,c("id_motu")]


hh <- st_union(st_union(st_union(l_shp$ocean,l_shp$blue_lagoon),land),l_shp$lagoon)
hh <- st_as_sf(st_union(hh))

l_shp$shallow  <-  st_difference(l_shp$atoll,hh)
l_shp$shallow$habitat  <-  "shallow"
l_shp$shallow <- st_cast(l_shp$shallow, "POLYGON")
l_shp$shallow <- st_make_valid(l_shp$shallow)



shp_loc <- st_union(l_shp$reef_buffer,l_shp$bush)
shp_loc <- st_union(shp_loc,l_shp$forest_dense)
shp_loc <- st_union(shp_loc,l_shp$forest_clairseme_buffer)
shp_loc <- st_union(shp_loc,l_shp$mudflat)
shp_loc <- st_union(shp_loc,l_shp$rock)
shp_loc <-  st_difference(land,shp_loc)
shp_loc  <- st_as_sf(st_union(shp_loc))
shp_loc$habitat  <-  "beach"
l_shp$beach <- shp_loc
l_shp$beach <- st_cast(l_shp$beach, "POLYGON")
l_shp$beach <- st_make_valid(l_shp$beach)


shp_loc_1 <- st_union(l_shp$bush,l_shp$forest_dense)
shp_loc <- st_union(shp_loc_1,l_shp$forest_clairseme_buffer)
shp_loc <- st_union(shp_loc,l_shp$mudflat)
l_shp$reef  <-  st_difference(l_shp$reef_buffer,shp_loc)
l_shp$reef <- l_shp$reef[,c("habitat")]
l_shp$reef$habitat <- "reef"
l_shp$reef <- st_cast(l_shp$reef, "POLYGON")
l_shp$reef <- st_make_valid(l_shp$reef)


shp_loc <- st_union(shp_loc_1,l_shp$mudflat)
l_shp$sparse_forest  <-  st_difference(l_shp$forest_clairseme_buffer,shp_loc )
l_shp$sparse_forest <- l_shp$sparse_forest[,c("habitat")]
l_shp$sparse_forest$habitat <- "sparse_forest"
l_shp$sparse_forest <- st_cast(l_shp$sparse_forest, "POLYGON")
l_shp$sparse_forest <- st_make_valid(l_shp$sparse_forest)

shp_loc <- st_union(l_shp$sparse_forest,l_shp$forest_dense)
l_shp$rocks  <-  st_difference(l_shp$rocks,shp_loc )
l_shp$rocks <- l_shp$rocks[,c("habitat")]
l_shp$rocks$habitat <- "rocks"
l_shp$rocks <- st_cast(l_shp$rocks, "POLYGON")
l_shp$rocks <- st_make_valid(l_shp$rocks)


shp_loc <- st_union(l_shp$reef,l_shp$forest_dense)
shp_loc <- st_union(shp_loc,l_shp$sparse_forest)
shp_loc <- st_union(shp_loc,l_shp$rocks)
l_shp$bush  <-  st_difference(l_shp$bush,shp_loc)
l_shp$bush <- l_shp$bush[,c("habitat")]
l_shp$bush <- st_cast(l_shp$bush, "POLYGON")
l_shp$bush <- st_make_valid(l_shp$bush)

l_shp$dense_forest <- l_shp$forest_dense
l_shp$dense_forest <- st_cast(l_shp$dense_forest, "POLYGON")
l_shp$dense_forest <- st_make_valid(l_shp$dense_forest)

vec_habitat <-c("bush","sparse_forest","dense_forest","mudflat","reef","rocks","beach")

rangi <- l_shp$ocean

for(h in vec_habitat) {
    add_shp  <-  l_shp[[h]]
    rangi <- rbind(rangi,add_shp)
}

rangi <- st_make_valid(rangi)
plot(rangi)



rangi_atoll <- st_intersection(rangi,land)

rangi_atoll <- st_make_valid(rangi_atoll)


vec_habitat_atoll <-c("blue_lagoon","lagoon","ocean","shallow")
for(h in vec_habitat_atoll) {
    add_shp  <-  l_shp[[h]]
    add_shp$id_motu <- 0
    add_shp <- add_shp[,colnames(rangi_atoll)]
    rangi_atoll <- rbind(rangi_atoll,add_shp)
}


plot(rangi_atoll)
rangi_atoll <- st_cast(rangi_atoll, to = "POLYGON")
rangi_atoll <- st_make_valid(rangi_atoll)

st_write(rangi_atoll, "SIG/rangi.shp")

st_write(rangi, "SIG/rangi_atoll.shp")

plot(l_shp$sparse_forest)

l_shp$sparse_forest
str(l_shp)
