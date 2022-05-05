
vecPackage=c("lubridate","ggplot2","moveVis","data.table","suncalc","anytime")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
    library(p,character.only = TRUE)

}

d <- read.csv("data/courlis_all.csv")
setDT(d)

d[,timestamp := gsub("T"," ",timestamp)]
d[,timestamp := gsub("Z","",timestamp)]
d[,datetime := as.POSIXct(anytime(timestamp) )]
d[,datetime :=  force_tz(datetime, tzone = "UTC")]
d[,date := as.Date(datetime)]
head(d)

source("functions/fun_daynight.r")

dd <- setDF(d)
d <- daynight(dd, colname_datetime = "datetime",colname_date = "date", colname_latitude = "location_lat", colname_longitude = "location_long")

fwrite(d,"data/courlis_all_daynight.csv")

