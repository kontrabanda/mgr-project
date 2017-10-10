library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)

polandCounties <- shapefile("../../data/Polska/powiaty/powiaty.shp")
polandCounties <- spTransform(polandCounties, CRS("+init=epsg:4326"))

poland <- shapefile("../../data/Polska/woj/województwa.shp")
poland <- spTransform(poland, CRS("+init=epsg:4326"))

crime <- read.csv("../../data/Polska/zdarzenia_rsow.csv", sep = "|")
crime <- crime[!is.na(crime$LAT)&!is.na(crime$LNG), ]

# zmiana z , na . w danych (inaczej traktowane są jako string)
crime$LAT <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LAT"]])))
crime$LNG <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LNG"]])))


#### statystyki podstawowe
nrow(crime)
ncol(crime)
####

#### statystyki podstawowe kategorii
length(unique(crime$KAT))
unique(crime$KAT)
####

#### maksymalna i minimalna data
crime[which.max(crime$DATA), ]
crime[which.min(crime$DATA), ]
####

coordinates(crime) =~ LNG+LAT
projection(crime) = projection(poland)

#### mapa przestępstw na tle województw
png('../plot/metadata/polska_województwa.png', width=2500, height=3000, res=300)
plot(poland, col='light blue')
points(crime, col='red', cex=.5, pch='+')
dev.off()
####

#### mapa zagregowanych przestępstw na tle gminy
dcounties <- aggregate(polandCounties, by='jpt_kod_je')
crimeWithCounties <- over(crime, dcounties)
crimeWithCounties <- cbind(data.frame(crime), crimeWithCounties)

sumByCounties <- function(x) {
  dat <- crimeWithCounties$jpt_kod_je == x
  sum(dat)
}

countynames <- unique(dcounties$jpt_kod_je)
sumOfCrimesByCountyName <- sapply(countynames, sumByCounties)
dotchart(tail(sort(sumOfCrimesByCountyName), 10), cex=0.65)

sumOfCrimesByCountyName1 <- data.frame(jpt_kod_je=names(sumOfCrimesByCountyName), sumOfCrimesByCountyName)
countiesWithSumOfCrimes <- merge(dcounties, sumOfCrimesByCountyName1, by='jpt_kod_je')

display.brewer.all()
my.palette <- brewer.pal(n = 9, name = "YlOrBr")

png('../plot/metadata/polska_gminy.png', width=2500, height=3000, res=300)
spplot(countiesWithSumOfCrimes, 'sumOfCrimesByCountyName', col.regions = my.palette, cuts = 8)
dev.off()
####

### liczba przestępstw z podziałem kategorii
categoryNames <- unique(crime$KAT)
sumByCategory <- function(x) {
  sum(crime$KAT == x)
}
crimeInCategories <- sapply(categoryNames, sumByCategory)
crimeInCategories <- data.frame(NAME=categoryNames, crimeInCategories)
