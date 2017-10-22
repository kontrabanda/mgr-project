library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(spatstat)
library(maptools)

# pobranie danych
crimeBoston <- read.csv("../../data/usa/crimes/crime_boston.csv", sep = ",")

# wyłączenie danych z brakującą lokacją
crimeBoston <- crimeBoston[!is.na(crimeBoston$Lat)&!is.na(crimeBoston$Long), ]

# boston granice
boston <- shapefile("../../data/usa/boundries/boston/Bos_neighborhoods.shp")
boston <- spTransform(boston, CRS("+init=epsg:4326"))
onlyBoston <- aggregate(boston)

coordinates(crimeBoston) =~ Long+Lat
projection(crimeBoston) = projection(boston)

# boston unikalne dane
crimeBoston <- intersect(crimeBoston, boston)
crimeBostonDF <- data.frame(crimeBoston)
crimeBostonDF = unique(crimeBostonDF[, 2:17])
crimeBoston <- crimeBostonDF
coordinates(crimeBoston) =~ Long+Lat
projection(crimeBoston) = projection(boston)

### Crime PPP object
onlyBostonOwin <- as.owin(onlyBoston)
crimePts <- coordinates(crimeBoston)
crimePPP <- ppp(crimePts[,1], crimePts[,2], window=onlyBostonOwin)
###

### k function
kCrime <- Kest(crimePPP)
kCrime
png('../plot/k_function/Boston/k_function_ALL.png', width=2500, height=3000, res=300)
plot(kCrime, main="funkcja K dla całego Bostonu")
dev.off()
###

### wyciągnięcie losowych 1000 pktów
#nie udaję się w sensownym czasie
#set.seed(12)
#pts <- crimePts[sample(nrow(crimePts), 1000), ]
#pppRandom <- ppp(pts[,1], pts[,2], window=onlyBostonOwin)
#kest.envelope <- envelope(pppRandom, Kest, nsim = 6)
#png('../plot/k_function/Boston/envelope_k_function_ALL.png', width=2500, height=3000, res=300)
#plot(kest.envelope, main="Envelope funkcja K dla całego Bostonu")
#dev.off()
###