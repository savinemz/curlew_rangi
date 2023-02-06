library(sf); library(dplyr)
library(data.table)
library(DHARMa)

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





#setDT(rangi_rat)
#calcul des occurences
sum_loc_rat[,date_j := as.numeric(format(as.Date(date),"%j"))]
sum_loc_rat[,first_date_j := min(date_j),by = .(bird_id)]
sum_loc_rat[,beacon_age_j := date_j - first_date_j]
sum_loc_motus <- sum_loc_rat [,.(occurence =.N), by =.(id_poly, day_night, bird_id,beacon_age_j)]


#ajout de la durée de vie des balises
bird_motu <- unique(sum_loc_rat[,.(id_motu, bird_id,beacon_age_j)])
poly_motu <- rangi_rat[,.(id_motu, id_poly)]
bird_poly <- merge(bird_motu, poly_motu, by= "id_motu", allow.cartesian= T)


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



tab_glmm_i[habitat == " reef",habitat := "reef"]




# glmm de ref dans le rapport ziformula = day_night
##library(glmm)
library(glmmTMB)

# Original model
glmm <- glmmTMB(occurence~ area_poly_st + habitat*day_night + rats*day_night +  (1|id_motu) + (1|bird_id),
                family = "poisson", data=tab_glmm_i[habitat != "mudflat",])
sglmm <- summary(glmm)
print(sglmm)
simulationOutput <- simulateResiduals(fittedModel = glmm, plot = F)
testZeroInflation(simulationOutput)
plot(simulationOutput)



# Model without an interaction
glmm2 <- glmmTMB(occurence~ area_poly_st + habitat + day_night + rats +  (1|id_motu) + (1|bird_id),
                family = "poisson", data=tab_glmm_i[habitat != "mudflat",])
sglmm2 <- summary(glmm2)
print(sglmm2)



# Likelihood ratio test
anova(glmm, glmm2, test="LRT")

## model with interaction is significantly better than without


# Effet des rats sur la présence des courlis sur chaque habitats
library(ggeffects)
ggpred <- ggpredict(glmm,terms = c("habitat","rats","day_night"))
print(ggpred)
plot(ggpred)


ggpred <- ggpredict(glmm,terms = c("habitat"))
print(ggpred)
plot(ggpred)

## pour interaction pent mais ici ne fonctinne pas
## library(effects)
## plot(predictorEffects(glmm))


library(multcomp)
summary(glht(glmm2))
                                        # Histogrammes des occurrences


TukeyHSD(aov(occurence~ habitat*day_night + rats*day_night ,data=tab_glmm_i[habitat != "mudflat",]))

tuk <- TukeyHSD(aov(occurence~ habitat,data=tab_glmm_i[habitat != "mudflat",]))
plot(tuk)
