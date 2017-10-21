library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(spatstat)
library(maptools)

# pobranie danych
crimeBoston <- read.csv("../../data/usa/crimes/crime_boston.csv", sep = ",")
nrow(crimeBoston)

# wyłączenie danych z brakującą lokacją
crimeBoston <- crimeBoston[!is.na(crimeBoston$Lat)&!is.na(crimeBoston$Long), ]
nrow(crimeBoston)

# boston granice
boston <- shapefile("../../data/usa/boundries/boston/Bos_neighborhoods.shp")
boston <- spTransform(boston, CRS("+init=epsg:4326"))
plot(boston)

coordinates(crimeBoston) =~ Long+Lat
projection(crimeBoston) = projection(boston)

# przestępstwa w granicach Bostonu
crimeBoston <- intersect(crimeBoston, boston)
crimeBostonDF <- data.frame(crimeBoston)
nrow(crimeBostonDF)

# liczba unikalnych danych
nrow(unique(crimeBostonDF))

# liczba unikalnych danych po wyłączeniu id
nrow(unique(crimeBostonDF[, 2:17]))
crimeBostonDF = unique(crimeBostonDF[, 2:17])

crimeBoston <- crimeBostonDF
coordinates(crimeBoston) =~ Long+Lat
projection(crimeBoston) = projection(boston)

# kategorie przestępstw
categoryNames <- unique(crimeBostonDF$OFFENSE_CODE_GROUP)
length(categoryNames)
categoryNames

sumByCategory <- function(x) {
  sum(crimeBostonDF$OFFENSE_CODE_GROUP == x)
}

crimeInCategories <- sapply(categoryNames, sumByCategory)
crimeInCategories <- data.frame(NAME=categoryNames, crimeInCategories)
sum(crimeInCategories$crimeInCategories)

sortedCrimesInCategories <- crimeInCategories[order(crimeInCategories[,2]), ]
crimeInCategories[order(crimeInCategories[,2], decreasing = T), ]

png('../plot/metadata/boston/boston_przestępstwa_po_typie.png', width=2500, height=3000, res=300)
dotchart(x=sortedCrimesInCategories[,2], labels=sortedCrimesInCategories[,1], cex=0.65)
dev.off()

# przestępstwa jako punkty na mapie
onlyBoston <- aggregate(boston)
bostonOwin <- as.owin(onlyBoston)
pts <- coordinates(crimeBoston)
p <- ppp(pts[,1], pts[,2], window = bostonOwin)
png('../plot/metadata/boston/boston_przestepstwa.png', width=2500, height=3000, res=300)
plot(p)
dev.off()

# dotchart dla osiedli
settlements <- aggregate(boston, by='Name')
plot(settlements)
crimeWithSettlements <- over(crimeBoston, settlements)
crimeWithSettlements <- cbind(data.frame(crimeBoston), crimeWithSettlements)

sumBySettlements <- function(x) {
  dat <- crimeWithSettlements$Name == x
  sum(dat)
}

settlementsnames <- unique(settlements$Name)
sumOfCrimesBySettlementName <- sapply(settlementsnames, sumBySettlements)

png('../plot/metadata/boston/boston_osiedla_dotchart.png', width=2500, height=3000, res=300)
dotchart(tail(sort(sumOfCrimesBySettlementName), 10), cex=0.65)
dev.off()

# 
sumOfCrimesBySettlementName1 <- data.frame(jpt_nazwa_=names(sumOfCrimesBySettlementName), sumOfCrimesBySettlementName)
settlementsWithSumOfCrimes <- merge(settlements, sumOfCrimesBySettlementName1, by='jpt_nazwa_')

my.palette <- brewer.pal(n = 9, name = "YlOrBr")

png('../plot/metadata/bournemouth/bournemouth_osiedla.png', width=2500, height=3000, res=300)
spplot(settlementsWithSumOfCrimes, 'sumOfCrimesBySettlementName', col.regions = my.palette, cuts = 8)
dev.off()









