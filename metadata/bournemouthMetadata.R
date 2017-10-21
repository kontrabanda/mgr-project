library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)

# pobranie danych 
crimeDorset <- read.csv("../../data/gb/crimes/dorset/dorset_all.csv", sep = ",")

# dane tylko z Latitude and Longitude
crimeDorset <- crimeDorset[!is.na(crimeDorset$Latitude)&!is.na(crimeDorset$Longitude), ]

# liczba unikalnych danych
nrow(unique(crimeDorset))

# liczba unikalnych danych po wyłączeniu id
nrow(unique(crimeDorset[, 2:12]))

length(unique(crimeDorset$Crime.type))

################## bournemouth
bournemouth <- shapefile("../../data/gb/boundries/Bournemouth.shp")
bournemouth <- spTransform(bournemouth, CRS("+init=epsg:4326"))
plot(bournemouth)

coordinates(crimeDorset) =~ Longitude+Latitude
projection(crimeDorset) = projection(bournemouth)

crimeBournemouth <- intersect(crimeDorset, bournemouth)
crimeBournemouthDF <- data.frame(crimeBournemouth)

# liczba unikalnych danych
nrow(unique(crimeBournemouthDF))

# liczba unikalnych danych po wyłączeniu id
nrow(unique(crimeBournemouthDF[, 2:12]))

length(unique(crimeBournemouthDF$Crime.type))

# only unique values
crimeBournemouthDF <- unique(crimeBournemouthDF[, 2:12])

#### liczba przestępstw z podziałem kategorii
categoryNames <- unique(crimeBournemouthDF$Crime.type)

sumByCategory <- function(x) {
  sum(crimeBournemouthDF$Crime.type == x)
}

crimeInCategories <- sapply(categoryNames, sumByCategory)
crimeInCategories <- data.frame(NAME=categoryNames, crimeInCategories)
sum(crimeInCategories$crimeInCategories)

sortedCrimesInCategories <- crimeInCategories[order(crimeInCategories[,2]), ]
png('../plot/metadata/bournemouth/bournemouth_przestępstwa_po_typie.png', width=2500, height=3000, res=300)
dotchart(x=sortedCrimesInCategories[,2], labels=sortedCrimesInCategories[,1], cex=0.65)
dev.off()
####

#### stworzenie mapy Bournemouth z przestępstwami jako pkt
png('../plot/metadata/bournemouth/bournemouth_przestepstwa.png', width=2500, height=3000, res=300)
plot(bournemouth, col='light green')
points(crimeBournemouth, col='red', cex=1, pch='x')
dev.off()
####

#### stworzenie mapy z agregacją do poziomu osiedla
settlements <- aggregate(bournemouth, by='name')
plot(settlements)
crimeWithSettlements <- over(crimeBournemouth, settlements)
crimeWithSettlements <- cbind(data.frame(crimeBournemouth), crimeWithSettlements)

sumBySettlements <- function(x) {
  dat <- crimeWithSettlements$name == x
  sum(dat)
}

settlementsnames <- unique(settlements$name)
sumOfCrimesBySettlementName <- sapply(settlementsnames, sumBySettlements)

png('../plot/metadata/bournemouth/bournemouth_osiedla_dotchart.png', width=2500, height=3000, res=300)
dotchart(tail(sort(sumOfCrimesBySettlementName), 10), cex=0.65)
dev.off()

sumOfCrimesBySettlementName1 <- data.frame(name=names(sumOfCrimesBySettlementName), sumOfCrimesBySettlementName)
settlementsWithSumOfCrimes <- merge(settlements, sumOfCrimesBySettlementName1, by='name')

my.palette <- brewer.pal(n = 9, name = "YlOrBr")

png('../plot/metadata/bournemouth/bournemouth_osiedla.png', width=2500, height=3000, res=300)
spplot(settlementsWithSumOfCrimes, 'sumOfCrimesBySettlementName', col.regions = my.palette, cuts = 8)
dev.off()
####


















