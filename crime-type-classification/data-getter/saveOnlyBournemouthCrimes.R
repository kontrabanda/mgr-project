library(raster)
library(sp)

bournemouth <- shapefile("../../data/gb/boundries/Bournemouth.shp")
bournemouth <- spTransform(bournemouth, CRS("+init=epsg:4326"))

onlyBournemouth <- aggregate(bournemouth)

crime <- read.csv("../../data/gb/crimes/dorset/dorset_all.csv")
crime <- crime[!is.na(crime$Latitude)&!is.na(crime$Longitude), ]
coordinates(crime) =~ Longitude+Latitude
projection(crime) = projection(bournemouth)

crimeBournemouth <- intersect(crime, onlyBournemouth)
crimeBournemouthDF <- data.frame(crimeBournemouth)

write.csv(crimeBournemouthDF, file = "../../data/gb/crimes/crime_only_bournemouth.csv")
