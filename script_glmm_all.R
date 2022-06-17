library(sf); library(dplyr)
library(data.table)

#Donnees balise Icarus #################################################################################################################################################

#calculs des surfaces par polygones
#rangi_rat <- st_read("SIG/rangi_atoll.shp")
rangi_rat <- st_read("SIG/rangi_motu.shp")
rangi_rat$area_poly <- st_area(rangi_rat)

#calcul des surfaces total par habitat
area_habitat <- aggregate(area_poly~habitat, rangi_rat, sum)


#calcul des surfaces par motu (methode vecteur)
area_motu <- aggregate(area_poly~id_motu, rangi_rat, sum)
names(area_motu)[2] <- "area_motu"
rangi_rat <- merge(rangi_rat, area_motu, by = "id_motu")


#identifiant des polygones
rangi_rat$id_poly <- 1: nrow(rangi_rat)
rangi_rat <- rangi_rat %>% relocate(id_poly, .after = id_motu)

#proportion des habitats par motu
rangi_rat$proportion <- (rangi_rat$area_poly/rangi_rat$area_motu)
rangi_rat <- rangi_rat %>% relocate(proportion, .after = area_motu)






#localisation des courlis
loc_courlis <- read.csv("Courlis_all_daynight/courlis_all_daynight.csv")
loc_courlis <- subset(loc_courlis,location_long < 0)


# # Suppression des donnees OrniTrack
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C27")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C32")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C33")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C34")
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C40")

#suppression de l'individu C09 (donnees insuffisantes)
loc_courlis <- subset(loc_courlis, loc_courlis$bird_id != "C09")

## ajout de la colonne date (directement par RL dans les nouvelles donnees) et la colonne heure manuellement
loc_courlis$heure_HH <- substr(loc_courlis$timestamp,12,13)
loc_courlis$date_HH <- paste0(loc_courlis$date, "_", loc_courlis$heure_HH)


# Transformation des coordonnees en donnees spatiales + modification de la projection
courlis_sf <- st_as_sf(loc_courlis, coords = c("location_long","location_lat"))
st_crs(courlis_sf) <- 4326
courlis_sf <- st_transform(courlis_sf,crs=3832)
sum_loc_rat <- st_intersection(rangi_rat, courlis_sf)




setDT(sum_loc_rat)
sum_loc_poly_rat <- sum_loc_rat[,.(occurence = .N),by=.(id_poly,bird_id,date_HH)][,.(occurence = .N),by=.(id_poly)]

rangi_rat <- merge(rangi_rat, sum_loc_poly_rat, bx = "id_poly", all.x = T)
rangi_rat$occurence[is.na(rangi_rat$occurence)] <- 0
rangi_rat <- rangi_rat %>% relocate(occurence, .after = habitat)





## GLMM (Icarus) ###################################################################################################################################################

setDT(rangi_rat)
#calcul des occurences
sum_loc_motus <- sum_loc_rat [,.(occurence =.N), by =.(id_poly, day_night, bird_id)]

#ajout des absences utilise par individu = permet de definir le domaine vital (DV)= zone utilise pour toutes les activites
bird_motu <- unique(sum_loc_rat[,.(id_motu, bird_id)])
poly_motu <- rangi_rat[,.(id_motu, id_poly)]
bird_poly <- merge(bird_motu, rangi_rat[,.(id_motu, id_poly)], by= "id_motu", allow.cartesian= T)

#ajout daymight
#bird_daynight <- rbind (bird_poly[,daynight :="day"], bird_poly[,daynight :="night"])
setDF(bird_poly)
bird_daynight_day <- bird_poly
setDT(bird_daynight_day)
bird_daynight_day [,day_night := "day"]

bird_daynight_night <- bird_poly
setDT(bird_daynight_night)
bird_daynight_night [,day_night := "night"]

setDT(bird_poly)
bird_daynight <- bind_rows(bird_daynight_day, bird_daynight_night)



sum_loc_motus <- merge(sum_loc_motus, bird_daynight, all =T, allow.cartesian= T)
setDT(sum_loc_motus)
sum_loc_motus$occurence[is.na(sum_loc_motus$occurence)] <- 0
sum_loc_motus <- sum_loc_motus %>% relocate(id_motu, .after = id_poly)
sum_loc_motus <- sum_loc_motus %>% relocate(id_poly, .after = id_motu)

tab_glmm_i <- merge(sum_loc_motus, rangi_rat[,c(1,3,5,6)])
tab_glmm_i[,balise := "icarus"]
tab_glmm_i[,area_poly := as.numeric(area_poly)]
tab_glmm_i[,area_poly_st := scale(area_poly)]
tab_glmm_i[,rats := as.factor(rat == 1)]




# glmm de ref dans le rapport ziformula = day_night
library(glmm)
library(glmmTMB)
glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night ,data=tab_glmm_i[habitat != "mudflat",])
sglmm <- summary(glmm)
print(sglmm)



# ziformula = daynight et bird_id
glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night + bird_id, data=tab_glmm_i[habitat != "mudflat",])
sglmm <- summary(glmm)
print(sglmm)










#Donnees balise OrniTrack #################################################################################################################################################

#calculs des surfaces par polygones
#rangi_rat <- st_read("SIG/rangi_atoll.shp")
rangi_rat <- st_read("SIG/rangi_motu.shp")
rangi_rat$area_poly <- st_area(rangi_rat)

#calcul des surfaces total par habitat
area_habitat <- aggregate(area_poly~habitat, rangi_rat, sum)


#calcul des surfaces par motu (methode vecteur)
area_motu <- aggregate(area_poly~id_motu, rangi_rat, sum)
names(area_motu)[2] <- "area_motu"
rangi_rat <- merge(rangi_rat, area_motu, by = "id_motu")


#identifiant des polygones
rangi_rat$id_poly <- 1: nrow(rangi_rat)
rangi_rat <- rangi_rat %>% relocate(id_poly, .after = id_motu)

#proportion des habitats par motu
rangi_rat$proportion <- (rangi_rat$area_poly/rangi_rat$area_motu)
rangi_rat <- rangi_rat %>% relocate(proportion, .after = area_motu)






#localisation des courlis
loc_courlis <- read.csv("Courlis_all_daynight/courlis_all_daynight.csv")
loc_courlis <- subset(loc_courlis,location_long < 0)

# Suppression des donnees ICARUS
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



## ajout de la colonne date (directement par RL dans les nouvelles donnees) et la colonne heure manuellement
loc_courlis$heure_HH <- substr(loc_courlis$timestamp,12,13)
loc_courlis$date_HH <- paste0(loc_courlis$date, "_", loc_courlis$heure_HH)


# Transformation des coordonnees en donnees spatiales + modification de la projection
courlis_sf <- st_as_sf(loc_courlis, coords = c("location_long","location_lat"))
st_crs(courlis_sf) <- 4326
courlis_sf <- st_transform(courlis_sf,crs=3832)
sum_loc_rat <- st_intersection(rangi_rat, courlis_sf)

setDT(sum_loc_rat)
sum_loc_poly_rat <- sum_loc_rat[,.(occurence = .N),by=.(id_poly,bird_id)][,.(occurence = .N),by=.(id_poly)]

rangi_rat <- merge(rangi_rat, sum_loc_poly_rat, bx = "id_poly", all.x = T)
rangi_rat$occurence[is.na(rangi_rat$occurence)] <- 0
rangi_rat <- rangi_rat %>% relocate(occurence, .after = habitat)





## GLMM (OrniTrack day_night) ###################################################################################################################################################


setDT(rangi_rat)
#calcul des occurences
sum_loc_motus <- sum_loc_rat [,.(occurence =.N), by =.(id_poly, day_night, bird_id)]

#ajout des absences utilise par individu = permet de definir le domaine vital (DV)= zone utilise pour toutes les activites
bird_motu <- unique(sum_loc_rat[,.(id_motu, bird_id)])
poly_motu <- rangi_rat[,.(id_motu, id_poly)]
bird_poly <- merge(bird_motu, rangi_rat[,.(id_motu, id_poly)], by= "id_motu", allow.cartesian= T)

#ajout daymight
#bird_daynight <- rbind (bird_poly[,daynight :="day"], bird_poly[,daynight :="night"])
setDF(bird_poly)
bird_daynight_day <- bird_poly
setDT(bird_daynight_day)
bird_daynight_day [,day_night := "day"]

bird_daynight_night <- bird_poly
setDT(bird_daynight_night)
bird_daynight_night [,day_night := "night"]

setDT(bird_poly)
bird_daynight <- bind_rows(bird_daynight_day, bird_daynight_night)



sum_loc_motus <- merge(sum_loc_motus, bird_daynight, all =T, allow.cartesian= T)
setDT(sum_loc_motus)
sum_loc_motus$occurence[is.na(sum_loc_motus$occurence)] <- 0
sum_loc_motus <- sum_loc_motus %>% relocate(id_motu, .after = id_poly)
sum_loc_motus <- sum_loc_motus %>% relocate(id_poly, .after = id_motu)

tab_glmm_o <- merge(sum_loc_motus, rangi_rat[,c(1,3,5,6)])
tab_glmm_o[,balise := "orniTrack"]
tab_glmm_o[,area_poly := as.numeric(area_poly)]
tab_glmm_o[,area_poly_st := scale(area_poly)]
tab_glmm_o[,rats := as.factor(rat == 1)]




# glmm de ref dans le rapport ziformula = day_night
library(glmm)
library(glmmTMB)
glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night ,data=tab_glmm_o[habitat != "mudflat",])
sglmm <- summary(glmm)
print(sglmm)


# ziformula = day_night + bird_id
glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night + bird_id ,data=tab_glmm_o[habitat != "mudflat",])
sglmm <- summary(glmm)
print(sglmm)



# Effet des rats sur la présence des courlis sur chaque habitats
library(ggeffects)
ggpred <- ggpredict(glmm,terms = c("habitat","rats"))
print(ggpred)
#png("Rplot/glmm.png", width= 300, height = 300)
plot(ggpred)
#dev.off()


# Effet du jour et de la nuit sur la présence des courlis sur chaque habitats
ggpred <- ggpredict(glmm,terms = c("habitat","day_night"))
print(ggpred)
plot(ggpred)



# Histogrammes des occurrences
gg <- ggplot(data = tab_glmm, aes (x= occurence)) + facet_wrap(.~habitat, scales = "free") + geom_histogram()
gg
#ggsave("Rplot/histo_occ.png",gg)



library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = glmm, plot = F)
plot(simulationOutput)






#Fusion des donnees balises I&O #################################################################################################################################################


tab_glmm <- bind_rows(tab_glmm_i, tab_glmm_o)
setDT(tab_glmm)
tab_glmm[,balises := as.factor(balise == "icarus")]

## GLMM i&o ###################################################################################################################################################

# glmm balise o+i
glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + balises + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night + bird_id ,data=tab_glmm[habitat != "mudflat",])
 sglmm <- summary(glmm)
print(sglmm)

