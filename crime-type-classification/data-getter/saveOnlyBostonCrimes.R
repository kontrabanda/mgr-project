library(raster)
library(sp)

boston <- shapefile("../../../data/usa/boundries/boston/Bos_neighborhoods.shp")
boston <- spTransform(boston, CRS("+init=epsg:4326"))
onlyBoston <- aggregate(boston)

crime <- read.csv("../../../data/usa/crimes/crime_boston.csv")
crime <- crime[!is.na(crime$Lat)&!is.na(crime$Long), ]
coordinates(crime) =~ Long+Lat
projection(crime) = projection(boston)

crimeBoston <- intersect(crime, onlyBoston)
crimeBostonDF <- data.frame(crimeBoston)
write.csv(crimeBostonDF, file = "../../../data/usa/crimes/crime_only_boston.csv")
