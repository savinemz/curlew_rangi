
### function to add the column daynight in data
### the data should to have some column
### datetime, latitude, longitude

daynight <- function(d, colname_datetime = "datetime_UTC",colname_date = "date", colname_latitude = "latitude_wgs84", colname_longitude = "longitude_wgs84",convert_time = FALSE) {


   ## ---- initializing parameters for debugging ----
##   colname_datetime = "UTC_datetime"; colname_date = "UTC_date"; colname_latitude = "Latitude"; colname_longitude = "Longitude"
    ## ---

    require(maptools)
    require(data.table)

    setDT(d)


    ## checking colnames
    if(is.null(colname_date)) colname_date  <- ""
    colnames_ref <- c("datetime_UTC", "date","latitude_wgs84", "longitude_wgs84")
    colnames_data <- c(colname_datetime,colname_date,colname_latitude, colname_longitude)
    colnames_diff <- data.table(ref=colnames_ref,old = colnames_data)[ref != old & old != "",]
    ## print(colnames_diff)

    colnames_save <- intersect(colnames(d),colnames_ref)
    ##  print(colnames_save)

   # if(length(colnames_save) > 0) setnames(d,colnames_save,paste0("XXX_OLD_",colnames_save,"_OLD_"))
    if(nrow(colnames_diff) >0)  setnames(d,colnames_diff$old,colnames_diff$ref)

    ## head(d)

    d <- d[!is.na(date) & !is.na(datetime_UTC) & !is.na(longitude_wgs84) & !is.na(latitude_wgs84),]
    if(convert_time)  d[,date := as.POSIXct(date,tz = "UTC")]
    if(convert_time)  d[,datetime_UTC := as.POSIXct(strptime(datetime_UTC,"%Y-%m-%d %H:%M:%S"))]

    cat(nrow(d),"samples with valid date and time and location\n")
    print(head(d))
    coord <- as.matrix(d[,.(longitude_wgs84, latitude_wgs84)])
    datetimes <- d[,datetime_UTC]
    colnames(coord) <- c("x","y")
    coordinates(d) <- c("longitude_wgs84", "latitude_wgs84")
    lonlat <- SpatialPoints(coordinates(d),proj4string=CRS("+proj=longlat +datum=WGS84"))

    ##  using function form sun-methods{maptools}. They use algorithms provided by the National Oceanic & Atmospheric Administration (NOAA)
    cat("calculating sunrise...")

    d$sunrise  <- sunriset(crds = coord, dateTime = datetimes, direction="sunrise", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating sunset...")
    d$sunset  <- sunriset(crds = coord, dateTime = datetimes, direction="sunset", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating civil dawn...")
    d$dawn_civil <- crepuscule(crds = coord, dateTime = datetimes,solarDep=6, direction="dawn", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating civil dusk...")
    d$dusk_civil <- crepuscule(crds = coord, dateTime = datetimes,solarDep=6, direction="dusk", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating astronmical dawn...")
    d$dawn_astro <- crepuscule(crds = coord, dateTime = datetimes,solarDep=18, direction="dawn", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating astronomical dusk...")
    d$dusk_astro <- crepuscule(crds = coord, dateTime = datetimes,solarDep=18, direction="dusk", POSIXct=TRUE)$time
    cat("Done !\n")

     d <- as.data.table(d)

    cat("adding day_night column...")
    d[,day_night := ifelse(datetime_UTC >= sunrise & datetime_UTC <= sunset,"day","night")]
    cat("Done !\n")
    cat("adding day_crepuscule_civil_night column...")
    d[,day_crepuscule_civil_night :=  ifelse(day_night == "day","day",ifelse(d$datetime_UTC > dawn_civil & datetime_UTC < d$dusk_civil, "crepuscule_civil","night"))]
    cat("Done !\n")
    cat("adding day_crepuscule_astro_night column...")
    d[,day_crepuscule_astro_night :=  ifelse(day_crepuscule_civil_night %in% c("day","crepuscule_civil"),day_crepuscule_civil_night,ifelse(d$datetime_UTC > d$dawn_astro & d$datetime_UTC < d$dusk_astro, "crepuscule_astro","night"))]
    cat("Done !\n")

    ## backtransform colnames

    if(nrow(colnames_diff) >0) setnames(d,colnames_diff$ref,colnames_diff$old)
  #  if(length(colnames_save) > 0) setnames(d,paste0("XXX_OLD_",colnames_save,"_OLD_"),colnames_save)

    cat("==> Done !\n\n")
    return(d)

}

