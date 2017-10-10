library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(maptools)
library(spatstat)

bialystokSettlements <- shapefile("../../data/bialystok/bialystok.shp")
bialystokSettlements <- spTransform(bialystokSettlements, CRS("+init=epsg:4326"))
onlyBialystok <- aggregate(bialystokSettlements)

crime <- read.csv("../../data/Polska/zdarzenia_rsow.csv", sep = "|")
crime <- crime[!is.na(crime$LAT)&!is.na(crime$LNG), ]

# zmiana z , na . w danych (inaczej traktowane są jako string)
crime$LAT <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LAT"]])))
crime$LNG <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LNG"]])))

coordinates(crime) =~ LNG+LAT
projection(crime) = projection(bialystokSettlements)

## wybranie przestępstw tylko z Białegostoku
system.time(crimeBialystok <- intersect(crime, onlyBialystok))

onlyBialystokOwin <- as.owin(onlyBialystok)
crimePts <- coordinates(crimeBialystok)
crimePPP <- ppp(crimePts[,1], crimePts[,2], window=onlyBialystokOwin)

plot(crimePPP)

ds <- density(crimePPP)
plot(ds, main='gęstość przestępstw')
kCrime <- Kest(crimePPP)

png('../plot/k_function/k_function_ALL.png', width=2500, height=3000, res=300)
plot(kCrime, main="funkcja K dla całego Białegostoku")
dev.off()

## kategorie
kFunctionForCategory <- function(categoryName) {
  pts <- coordinates(crimeBialystok[crimeBialystok$KAT == categoryName, ])
  p <- ppp(pts[,1], pts[,2], window=onlyBialystokOwin)
  k <- Kest(p)
  filename <- paste('../plot/k_function/k_function', categoryName, sep='_')
  filename <- paste(filename, 'png', sep='.')
  png(filename = filename, width=2500, height=3000, res=300)
  plot(k, main=categoryName)  
  dev.off()
}

categoryNames <- unique(crimeBialystok@data$KAT)
kFunctionForCategory("CHU")     # 28 537
kFunctionForCategory("RD")      # 32 182
kFunctionForCategory("KRA")     # 13 479
#kFunctionForCategory("OŚ")      # 2 509
kFunctionForCategory("ALK")     # 928
#kFunctionForCategory("LEG")     # 1 185
kFunctionForCategory("BEZP")    # 8 292
kFunctionForCategory("ZWIE")    # 2 548
kFunctionForCategory("PORZ")    # 880
kFunctionForCategory("GOSP")    # 335


#sapply(categoryNames, kFunctionForCategory)

## kategorie z elavuate
set.seed(12)
kFunctionWithEvaluateForCategory <- function(categoryName, nsim = 5, count = 1000) {
  pts <- coordinates(crimeBialystok[crimeBialystok$KAT == categoryName, ])
  pts <- pts[sample(nrow(pts), count), ]
  pppInner <- ppp(pts[,1], pts[,2], window=onlyBialystokOwin)
  keInner <- envelope(pppInner, Kest, nsim = nsim)
  filename <- paste('../plot/k_function/k_function_evaluate', categoryName, sep='_')
  filename <- paste(filename, 'png', sep='.')
  png(filename = filename, width=2500, height=3000, res=300)
  plot(keInner, main=categoryName)  
  dev.off()
}

categoryNames <- unique(crimeBialystok@data$KAT)
kFunctionWithEvaluateForCategory("CHU", nsim=50)     # 28 537
kFunctionWithEvaluateForCategory("RD", nsim=50)      # 32 182
kFunctionWithEvaluateForCategory("KRA", nsim=50)     # 13 479
#kFunctionWithEvaluateForCategory("OŚ")      # 2 509
#kFunctionWithEvaluateForCategory("ALK")     # 928
#kFunctionWithEvaluateForCategory("LEG")     # 1 185
kFunctionWithEvaluateForCategory("BEZP")    # 8 292
kFunctionWithEvaluateForCategory("ZWIE")    # 2 548
#kFunctionWithEvaluateForCategory("PORZ")    # 880
#kFunctionWithEvaluateForCategory("GOSP")    # 335
##


## sample with envelope
set.seed(12)
sampleCrime <- crimeBialystok
crimePts <- coordinates(sampleCrime)
crimePts <- crimePts[sample(nrow(crimeBialystok), 1000), ]
crimePPP <- ppp(crimePts[,1], crimePts[,2], window=onlyBialystokOwin)

plot(crimePPP)
kCrime <- Kest(crimePPP)
plot(kCrime)
keCrime <- envelope(crimePPP, Kest, nsim = 5)
plot(keCrime)













