library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)

bialystok <- shapefile("../../data/bialystok/bialystok.shp")
bialystok <- spTransform(bialystok, CRS("+init=epsg:4326"))

crime <- read.csv("../../data/Polska/zdarzenia_rsow.csv", sep = "|")
crime <- crime[!is.na(crime$LAT)&!is.na(crime$LNG), ]

# zmiana z , na . w danych (inaczej traktowane są jako string)
crime$LAT <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LAT"]])))
crime$LNG <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LNG"]])))


coordinates(crime) =~ LNG+LAT
projection(crime) = projection(bialystok)

## wybranie przestępstw tylko z Białegostoku
crimeBialystok <- intersect(crime, bialystok)