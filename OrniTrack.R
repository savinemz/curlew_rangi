library(sf); library(dplyr)
library(data.table)


#calculs des surfaces par polygones
rangi <- st_read("SIG/rangi_motu.shp")
rangi <- rangi[,-3]
rangi$area_poly <- st_area(rangi)

#calcul des surfaces totales par habitat
area_habitat <- aggregate(area_poly~habitat, rangi, sum)


#calcul des surfaces par motu
area_motu <- aggregate(area_poly~id_motu, rangi, sum)
names(area_motu)[2] <- "area_motu"
rangi <- merge(rangi, area_motu, by = "id_motu")


#identifiant des polygones
rangi$id_poly <- 1: nrow(rangi)
rangi <- rangi %>% relocate(id_poly, .after = id_motu)

#proportion des habitats par motu
rangi$proportion <- (rangi$area_poly/rangi$area_motu)
rangi <- rangi %>% relocate(proportion, .after = area_motu)



#Analyse des donnees quantitative des motus
#rangi_quanti <- rangi
#rangi_quanti <- subset(rangi_quanti, rangi_quanti$id_motu != "0")

#area_motu_quanti <- area_motu
#area_motu_quanti <- subset(area_motu_quanti, area_motu_quanti$id_motu != "0")

#mean_area_motu <- mean(area_motu_quanti$area_motu)
#min_area_motu <- min(area_motu_quanti$area_motu)
#max_area_motu <- max(area_motu_quanti$area_motu)


#nb_hab_motus <- rangi_quanti[,.(nb_hab_motus = .N), by = .(id_motu)]
#mean_nb_hab_motu <- mean (nb_hab_motus$nb_hab_motus)
#min_nb_hab_motu <- min(nb_hab_motus$nb_hab_motus)
#max_nb_hab_motu <- max(nb_hab_motus$nb_hab_motus)

#surface de la zone d'etude (emmerge et immerge) puis uniquement la surface terrestre des motus
#sum(rangi$area_poly)
#sum_rangi <- rangi[ !(rangi$habitat %in% c("ocean","lagoon","blue_lagoon","shallow")),]
#sum(sum_rangi$area_poly)

#sum_mudflat_area <- sum(rangi_rat[habitat == "mudflat", area_poly])
#sum_all_area <- sum(rangi_rat[, area_poly])
#area_mudflat <- sum_mudflat_area/sum_all_area




## Analyse des donnees courlis OrniTrack ############################################################################################################################

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
sum_loc <- st_intersection(rangi, courlis_sf)



#nombre de donnes par heure par oiseau pour expliquer le choix des heures
setDT(loc_courlis)
nb_data_HH <- loc_courlis [,.(nb_data_HH = .N), by = .(bird_id, date_HH)]
nb_data_HH[,nb_data_HH := as.numeric(nb_data_HH)]
#bp <- boxplot (nb_data_HH$nb_data_HH ~ nb_data_HH$bird_id)


# figure nombre de données par heure par oiseaux
library(ggplot2)
gg <- ggplot(nb_data_HH,aes(x=nb_data_HH,y=bird_id)) + geom_violin()
gg <- gg + labs(y = "Bird_id", x = "Number of data per hour")
gg
#ggsave("Rplot/OrniTrack/nb_data_HH.png",gg)

#gg <- ggplot(nb_data_HH,aes(x=nb_data_HH)) + geom_histogram() + facet_grid(bird_id~.)
#gg










### Tableau description donnees courlis ########################################

library(data.table)
setDT(loc_courlis)
sum_courlis <- loc_courlis[,.(nb_data = .N,
                              first = min(date),
                              last = max(date)), by =.(bird_id)] # par oiseau: combien de donnees, premiere date et derniere date


sum_courlis[,duration_days := difftime(last, first, unit = "days")]#difference de temps entre la premiere et la derniere donnee. le ":=" veut dire pas de regroupement

nb_day <- loc_courlis [,.(j = 1), by = .(bird_id, date)] # regroupement par oiseau et par date
nb_day <- nb_day [,.(nb_day= .N), by = .(bird_id)] # nombre de jour de données 
sum_courlis <- merge(sum_courlis, nb_day, bx = "bird_id")
#fwrite(sum_courlis, "table/sum_courlis_OrniTrack.csv")


#nombre de donnees par oiseau par J/N
#sum_courlis_daynight <- loc_courlis[,.(nb_data = .N), by =.(bird_id, day_night)]
#fwrite(sum_courlis_daynight, "table/sum_courlis_daynight.csv")


#stat sur les donnees par heure par oiseau
#nombre de donnees par oiseau et par jour et par heure
#nb_data_j <- loc_courlis [,.(nb_data_jour = .N), by = .(bird_id, date, date_HH)]

#mean_data_j <- mean(nb_data_j$nb_data_jour)# moyenne du nombre de donnees par heure = 2 en arrondissant
#min_data_j <- min(nb_data_j$nb_data_jour)# le plus petit nombre de donnees par heure = 1
#max_data_j <- max(nb_data_j$nb_data_jour)# le plus grand nombre de donnees par heure = 4



### figure nombre de donnees par oiseau par jour par J/N ##########################################################################################################

nb_data_j_daynight <- loc_courlis [,.(nb_data_jour = .N), by = .(bird_id, date, day_night)]#comme on parle de J/N après, c'est interessant de savoir comment sont distribues les donnees
all_date <- expand.grid(bird_id = unique(loc_courlis[, bird_id]), date = seq(as.Date(min(loc_courlis$date)), as.Date(max(loc_courlis$date)),1), day_night = c("day", "night"))
all_date$date <- as.character(all_date$date)
nb_data_j_daynight <- merge(nb_data_j_daynight, all_date, by = c("bird_id","date", "day_night"), all = T)
nb_data_j_daynight$nb_data_jour[is.na(nb_data_j_daynight$nb_data_jour)] <- 0

library(ggplot2)
gg <- ggplot(data = nb_data_j_daynight, aes(x = as.Date(date), y = nb_data_jour, colour = day_night, group = day_night))
gg <- gg + geom_point()
gg <- gg + geom_line()
gg <- gg + facet_grid(bird_id~.)
gg <- gg + geom_point(data = subset(nb_data_j_daynight, nb_data_jour == 0), colour = "white", size = 0.8, alpha = 0.5)
gg <- gg + labs(x= "week", y= "number of localisation per week")
gg
#ggsave("Rplot/OrniTrack/nb_data_daynight.png",gg)


#stat sur les donnees sum_courlis
#nb_data_tot <- sum(sum_courlis$nb_data)# 2911 donnees

#median_nb_data <- median(sum_courlis$nb_data)# mediane du nombre de data par oiseau = 191
#mean_nb_data <- mean(sum_courlis$nb_data)# nombre moyen de data par oiseau = 291
#min_nb_data <- min(sum_courlis$nb_data) # le plus petit nombre de data = 2
#max_nb_data <- max(sum_courlis$nb_data) # le plus grand nombre de data = 635

#min_data_date <- min(sum_courlis$last)# gps a arreté d'emettre des le premier jour
#max_data_date <- max(sum_courlis$last)# donnee emise au maximum pendant 6 mois




## Utilisation du paysage par les oiseaux ##########################################################################################################

### Comparaison des habitats composant les motus occupes et non occupes ###################################################################################################

library(data.table)

#Trie des donnees
# regroupement du nombre d'occurence par polygone, par oiseau, par jour et par heure
setDT(sum_loc)
sum_loc_poly <- sum_loc[,.(occurence = .N),by=.(id_poly,bird_id)][,.(occurence = .N),by=.(id_poly)]

rangi <- merge(rangi, sum_loc_poly, bx = "id_poly", all.x = T)
rangi$occurence[is.na(rangi$occurence)] <- 0
rangi <- rangi %>% relocate(occurence, .after = habitat)


#création data.table pour d_gg
rangi_DT <- rangi
setDT(rangi_DT)
rangi_DT[,occupation := occurence>0]

#creation d'un tableau a partir de rangi_DT
rangi_DT[,proportion := as.numeric(proportion)]
d_gg <- rangi_DT[,.(prop_mean = mean(proportion),prop_med = median(proportion),inf95 = quantile(proportion, 0.025),sup95 = quantile(proportion, 0.975)), by=.(habitat,occupation)]



# Figure comparaison des habitats composant les motus occupes et non occupes
library(ggplot2); library(units)
gg <- ggplot(data = d_gg, aes(x = habitat, y = prop_mean,fill = occupation,colour=occupation,group=occupation))
gg <- gg + geom_errorbar(aes(ymin = inf95, ymax = sup95),width = 0.5,alpha=.5,size=1)
gg <- gg +  geom_point(alpha=.8,size=2)
gg <- gg + labs(y = "Proportion mean", x = "Habitats")
gg
ggsave("Rplot/OrniTrack/prop_mean.png",gg)







### Selection des habitats selon le jour ou la nuit #######################################################################

#Analyse sans daynight
#Distribution des localisations par habitat
distri_loc_hab <- sum_loc[,-c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27)]
distri_loc_hab <- distri_loc_hab %>% relocate(habitat, .after = bird_id)
distri_loc_hab <- distri_loc_hab %>% relocate(proportion, .after = habitat)



#  tableau occurence par habitat par oiseau
library(data.table)
setDT(distri_loc_hab)
tab_hab <- distri_loc_hab[,.(nb = .N),by=.(bird_id,habitat)]#.N = nombre de
tab_hab <- tab_hab[ !(habitat %in% c("ocean","lagoon","blue_lagoon","shallow")),]


# barre de reference : habitat de l'ensemble des motus
area_habitat <- aggregate(area_poly~habitat, rangi, sum)
area_habitat$proportion <- area_habitat$area_poly/sum(area_habitat$area_poly)
area_habitat <- area_habitat[,-c(2)]
setDT(area_habitat)
area_habitat[,bird_id := "habitat"]
area_habitat[,nb := as.numeric(proportion)]
area_habitat <- area_habitat[ !(habitat %in% c("ocean","lagoon","blue_lagoon","shallow")),]

setcolorder(area_habitat,c("bird_id","habitat","nb"))
tab_hab <- bind_rows(tab_hab, area_habitat)


# barre de reference : habitat par motus occupés
sum_loc_occ <- aggregate(occurence~id_motu, rangi_DT, sum)
sum_loc_occ <- subset(sum_loc_occ, !(occurence == 0))
colnames(sum_loc_occ)[2] <- "occurence_motu"

sum_loc_occ1 <- merge(rangi, sum_loc_occ, by = "id_motu")
area_motu_occ <- aggregate(area_poly~habitat, sum_loc_occ1, sum)
area_motu_occ$proportion <- area_motu_occ$area_poly/sum(area_motu_occ$area_poly)
area_motu_occ <- area_motu_occ[,-c(2)]
setDT(area_motu_occ)
area_motu_occ[,bird_id := "habitat_motu_occupe"]
area_motu_occ[,nb := as.numeric(proportion)]
area_motu_occ <- area_motu_occ[ !(habitat %in% c("ocean","lagoon","blue_lagoon","shallow")),]

tab_hab <- bind_rows(tab_hab, area_motu_occ)



#couleur par habitat
tab_fill  <- read.csv("library/colour_habitat.csv")
vec_fill <- tab_fill$colour
names(vec_fill) <- tab_fill$habitat

#nombre de donnees par oiseau
tab_bird <- distri_loc_hab[,.(nb = .N),by=bird_id]
tab_bird[,label := paste0(bird_id," (",nb,")")]

setDF(distri_loc_hab)


# representation graphique de la distribution des localisations par habitat et par habitat des motus occupes
ggdistrib <- ggplot(data = tab_hab,aes(x = nb, y = bird_id, fill = habitat))
ggdistrib <- ggdistrib + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib <- ggdistrib + scale_fill_manual(values = vec_fill)
ggdistrib <- ggdistrib + scale_y_discrete(breaks = c("habitat_motu_occupe", "habitat",tab_bird[,bird_id]),labels= c("habitat_motu_occupe", "habitat",tab_bird[,label]))
ggdistrib <- ggdistrib + labs(fill ="", y = "", x="")
ggdistrib
#ggsave("Rplot/OrniTrack/distrib_loc.png",gg)



# Analyse avec daynight
distri_loc_hab <- sum_loc[,-c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,24,25,26,27)]
distri_loc_hab <- distri_loc_hab %>% relocate(habitat, .after = bird_id)
distri_loc_hab <- distri_loc_hab %>% relocate(proportion, .after = habitat)
distri_loc_hab <- subset(distri_loc_hab, distri_loc_hab$bird_id != "C09")

library(data.table)
setDT(distri_loc_hab)
tab_daynight <- distri_loc_hab[,.(nb = .N),by=.(bird_id,habitat,day_night)]
tab_daynight <- tab_daynight[ !(habitat %in% c("ocean","lagoon","blue_lagoon","shallow")),]


setDF(area_habitat)
# barre de reference : habitat de l'ensemble des motus par J/N
area_habitat_day <- area_habitat
setDT(area_habitat_day)
area_habitat_day [,day_night := "day"]

area_habitat_night <- area_habitat
setDT(area_habitat_night)
area_habitat_night [,day_night := "night"]

setDT(area_habitat)

area_habitat_dn <- bind_rows(area_habitat_day, area_habitat_night)
tab_daynight <- bind_rows(tab_daynight, area_habitat_dn)





# barre de reference : habitat par motus occupés par J/N
sum_loc_daynight <- sum_loc[,.(occurence = .N),by=.(id_poly,bird_id,date_HH, day_night)][,.(occurence = .N),by=.(id_poly, day_night)]
rangi_daynight <- merge(rangi, sum_loc_daynight, by = "id_poly", all.x = T)
rangi_daynight$occurence[is.na(rangi_daynight$occurence)] <- 0
rangi_daynight <- subset(rangi_daynight, !(occurence.x == 0))

area_motu_occ_dn <- aggregate(area_poly~habitat + day_night, rangi_daynight, sum)
area_motu_occ_dn$proportion <- area_motu_occ_dn$area_poly/sum(area_motu_occ_dn$area_poly)
area_motu_occ_dn <- area_motu_occ_dn[,-c(3)]
setDT(area_motu_occ_dn)
area_motu_occ_dn[,bird_id := "habitat_motu_occupe"]
area_motu_occ_dn[,nb := as.numeric(proportion)]
area_motu_occ_dn <- area_motu_occ_dn[ !(habitat %in% c("ocean","lagoon","blue_lagoon","shallow")),]
area_motu_occ_dn <- area_motu_occ_dn %>% relocate(proportion, .after = nb)
area_motu_occ_dn <- area_motu_occ_dn %>% relocate(habitat, .after = bird_id)
area_motu_occ_dn <- area_motu_occ_dn %>% relocate(day_night, .after = habitat)

tab_daynight <- bind_rows(tab_daynight, area_motu_occ_dn)



#nombre de donnees par oiseau en fonction J/N
library(tidyr)
tab_bird_dn <- sum_loc[,.(occurence = .N),by=.(bird_id, day_night)]
tab_bird_dn <- subset(tab_bird_dn, tab_bird_dn$bird_id != "C09")
tab_bird_dn <- pivot_wider(tab_bird_dn, names_from = "day_night", values_from = "occurence")

setDT(tab_bird_dn)
tab_bird_dn[,label := paste0(bird_id," (",day," , ",night,")")]



# representation graphique de la distribution des localisations par habitat et par habitat des motus occupes en fonction J/N
ggdistrib <- ggplot(data = tab_daynight,aes(x = nb, y = bird_id, fill = habitat))+facet_grid(.~day_night)
ggdistrib <- ggdistrib + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib <- ggdistrib + scale_fill_manual(values = vec_fill)
ggdistrib <- ggdistrib + scale_y_discrete(breaks = c("habitat_motu_occupe", "habitat",tab_bird_dn[,bird_id]),labels= c("habitat_motu_occupe", "habitat",tab_bird_dn[,label]))
ggdistrib <- ggdistrib + labs(fill ="", y = "", x="")
ggdistrib
#ggsave("Rplot/distrib_loc_jn2.png",ggdistrib)








## Description du paysage ####################################################################################################################


### APC #############################################################################################################################################
# ACP à partir de la surface de chaque habitat par motu 

library("FactoMineR")
library("factoextra")
library(ade4)

#creation tableur pour l'ouverture des donnees numeriques => ACP
rangi_DT <- aggregate(area_poly~habitat + id_motu, rangi_DT, sum)
setDT(rangi_DT)
rangi_DT <- rangi_DT %>% relocate(habitat, .after = id_motu)
rangi_DT[,area_poly := as.numeric(area_poly)]


library(tidyr)
rangi_PCA <- rangi_DT
rangi_PCA <-pivot_wider(rangi_PCA, names_from = "habitat",
                        values_from = "area_poly")
rangi_PCA[is.na(rangi_PCA) == T] <- 0
row.names (rangi_PCA) <- rangi_PCA$id_motu
rangi_PCA <- rangi_PCA[,-1]
#rangi_PCA <- rangi_PCA[,-c(1,2,3,4)]

ACP <- PCA(rangi_PCA, scale.unit = TRUE, ncp = 5, graph = TRUE)

# graphique de corrélation des variables
fviz_pca_var(ACP, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "variable contribution",
             geom.ind = "point",
             repel = TRUE
)

ACP$var
#str(ACP)
# coord: Coordonnées
# Cos2: qualité de répresentation
# contrib: Contributions aux composantes principales

# extraction des valeurs propres et la proportion de variances retenues par les composantes principales
eig.val <- get_eigenvalue(ACP)
eig.val

# graphique des valeurs propres
plot_eigenvalue <- fviz_eig(ACP, addlabels = TRUE, ylim = c(0, 70))
plot_eigenvalue


# Contributions des variables à PC1
fviz_contrib(ACP, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(ACP, choice = "var", axes = 2, top = 10)






# Ajout donnees rats

library(sf); library(dplyr)
library(data.table)

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





## GLMM (OrniTrack) ###################################################################################################################################################


setDT(rangi_rat)
#calcul des occurences
sum_loc_motus <- sum_loc_rat [,.(occurence =.N), by =.(id_poly, bird_id)]

#ajout des absences utilise par individu = permet de definir le domaine vital (DV)= zone utilise pour toutes les activites
bird_motu <- unique(sum_loc_rat[,.(id_motu, bird_id)])
poly_motu <- rangi_rat[,.(id_motu, id_poly)]
bird_poly <- merge(bird_motu, rangi_rat[,.(id_motu, id_poly)], by= "id_motu", allow.cartesian= T)

#ajout daymight
#bird_daynight <- rbind (bird_poly[,daynight :="day"], bird_poly[,daynight :="night"])

sum_loc_motus <- merge(sum_loc_motus, bird_poly, by = c("id_poly","bird_id"), all =T, allow.cartesian= T)
setDT(sum_loc_motus)
sum_loc_motus$occurence[is.na(sum_loc_motus$occurence)] <- 0
sum_loc_motus <- sum_loc_motus %>% relocate(id_motu, .after = id_poly)
sum_loc_motus <- sum_loc_motus %>% relocate(id_poly, .after = id_motu)

tab_glmm_o <- merge(sum_loc_motus, rangi_rat[,c(1,3,5,6)])
tab_glmm_o[,balise := "orniTrack"]
tab_glmm_o[,area_poly := as.numeric(area_poly)]
tab_glmm_o[,area_poly_st := scale(area_poly)]
tab_glmm_o[,rats := as.factor(rat == 1)]

#somme occ muflat
#sum_mudflat <- sum(tab_glmm[habitat == "mudflat", occurence])
#sum_all <- sum(tab_glmm[, occurence])
#prop_mudflat <- sum_mudflat/sum_all






#Glmm
library(glmm)
library(glmmTMB)
glmm <- glmmTMB(occurence~habitat*rats + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~bird_id ,data=tab_glmm[habitat != "mudflat",], control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
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
#ggpred <- ggpredict(glmm,terms = c("habitat","day_night"))
#print(ggpred)
#plot(ggpred)



# Histogrammes des occurrences
gg <- ggplot(data = tab_glmm, aes (x= occurence)) + facet_wrap(.~habitat, scales = "free") + geom_histogram()
gg
#ggsave("Rplot/histo_occ.png",gg)



library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = glmm, plot = F)
plot(simulationOutput)












## GLMM (OrniTrack mais avec modele icarus comprenant day_night) ###################################################################################################################################################


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

#somme occ muflat
#sum_mudflat <- sum(tab_glmm[habitat == "mudflat", occurence])
#sum_all <- sum(tab_glmm[, occurence])
#prop_mudflat <- sum_mudflat/sum_all




tab_glmm <- bind_rows(tab_glmm_i, tab_glmm_o)




library(glmm)
library(glmmTMB)
glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night ,data=tab_glmm_o[habitat != "mudflat",])
sglmm <- summary(glmm)
print(sglmm)


# ziformula = 1
#glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~1 ,data=tab_glmm[habitat != "mudflat",])
#sglmm <- summary(glmm)
#print(sglmm)

# ziformula = day_night
#glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~day_night ,data=tab_glmm[habitat != "mudflat",])
#sglmm <- summary(glmm)
#print(sglmm)


# ziformula = bird_id
#glmm <- glmmTMB(occurence~habitat*day_night + rats*day_night + area_poly_st + (1|id_motu) + (1|bird_id), family = "nbinom2",ziformula = ~bird_id ,data=tab_glmm[habitat != "mudflat",])
#sglmm <- summary(glmm)
#print(sglmm)


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



