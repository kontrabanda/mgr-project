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

#### statystyki podstawowe
nrow(crimeBialystok@data)
ncol(crimeBialystok@data)
####

#### statystyki podstawowe kategorii
length(unique(crimeBialystok$KAT))
unique(crimeBialystok$KAT)
####

#### maksymalna i minimalna data
crimeBialystokDF <- data.frame(crimeBialystok)
crimeBialystokDF[which.max(crimeBialystok@data$DATA), ]
crimeBialystokDF[which.min(crimeBialystok@data$DATA), ]
####

#### stworzenie mapy Bialegostoku z przestępstwami jako pkt
png('../plot/metadata/bialystok_przestepstwa.png', width=2500, height=3000, res=300)
plot(bialystok, col='light green')
points(crimeBialystok, col='red', cex=1, pch='+')
dev.off()
####

#### stworzenie mapy z agregacją do poziomu osiedla
settlements <- aggregate(bialystok, by='jpt_nazwa_')
crimeWithSettlements <- over(crimeBialystok, settlements)
crimeWithSettlements <- cbind(data.frame(crimeBialystok), crimeWithSettlements)

sumBySettlements <- function(x) {
  dat <- crimeWithSettlements$jpt_nazwa_ == x
  sum(dat)
}

settlementsnames <- unique(settlements$jpt_nazwa_)
sumOfCrimesBySettlementName <- sapply(settlementsnames, sumBySettlements)

png('../plot/metadata/bialystok_osiedla_dotchart.png', width=2500, height=3000, res=300)
dotchart(tail(sort(sumOfCrimesBySettlementName), 10), cex=0.65)
dev.off()

sumOfCrimesBySettlementName1 <- data.frame(jpt_nazwa_=names(sumOfCrimesBySettlementName), sumOfCrimesBySettlementName)
settlementsWithSumOfCrimes <- merge(settlements, sumOfCrimesBySettlementName1, by='jpt_nazwa_')

my.palette <- brewer.pal(n = 9, name = "YlOrBr")

png('../plot/metadata/bialystok_osiedla.png', width=2500, height=3000, res=300)
spplot(settlementsWithSumOfCrimes, 'sumOfCrimesBySettlementName', col.regions = my.palette, cuts = 8)
dev.off()
####

#### liczba przestępstw z podziałem kategorii
categoryNames <- unique(crimeBialystokDF$KAT)
sumByCategory <- function(x) {
  sum(crimeBialystokDF$KAT == x)
}
crimeInCategories <- sapply(categoryNames, sumByCategory)
crimeInCategories <- data.frame(NAME=categoryNames, crimeInCategories)
####