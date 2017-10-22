library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(maptools)
library(spatstat)

### area
bournemouth <- shapefile("../../data/gb/boundries/Bournemouth.shp")
bournemouth <- spTransform(bournemouth, CRS("+init=epsg:4326"))
onlyBournemouth <- aggregate(bournemouth)
###

### crime
crimeDorset <- read.csv("../../data/gb/crimes/dorset/dorset_all.csv", sep = ",")
crimeDorset <- crimeDorset[!is.na(crimeDorset$Latitude)&!is.na(crimeDorset$Longitude), ]

coordinates(crimeDorset) =~ Longitude+Latitude
projection(crimeDorset) = projection(bournemouth)

crimeBournemouth <- intersect(crimeDorset, onlyBournemouth)
crimeBournemouthDF <- data.frame(crimeBournemouth)
crimeBournemouthDF <- unique(crimeBournemouthDF[, 2:12])

coordinates(crimeBournemouthDF) =~ Longitude+Latitude
crimeBournemouth <- crimeBournemouthDF
crimeBournemouthDF <- data.frame(crimeBournemouth)
###


### Crime PPP object
onlyBournemouthOwin <- as.owin(onlyBournemouth)
crimePts <- coordinates(crimeBournemouth)
crimePPP <- ppp(crimePts[,1], crimePts[,2], window=onlyBournemouthOwin)
png('../plot/metadata/bournemouth/bournemouth_przestepstwa_1.png', width=2500, height=3000, res=300)
plot(crimePPP)
dev.off()
###

### k function
kCrime <- Kest(crimePPP)
kCrime
png('../plot/k_function/Bournemouth/k_function_ALL.png', width=2500, height=3000, res=300)
plot(kCrime, main="funkcja K dla całego Bounemouth")
dev.off()

# wyciągnięcie losowych 1000 pktów
set.seed(12)
pts <- crimePts[sample(nrow(crimePts), 1000), ]
pppRandom <- ppp(pts[,1], pts[,2], window=onlyBournemouthOwin)
kest.envelope <- envelope(pppRandom, Kest, nsim = 10)
png('../plot/k_function/Bournemouth/envelope_k_function_ALL.png', width=2500, height=3000, res=300)
plot(kest.envelope, main="Envelope funkcja K dla całego Bounemouth")
dev.off()
###