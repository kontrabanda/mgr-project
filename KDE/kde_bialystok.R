library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(spatstat)

bialystok <- shapefile("../../data/bialystok/bialystok.shp")
bialystok <- spTransform(bialystok, CRS("+init=epsg:4326"))
onlyBialystok <- aggregate(bialystok)

crime <- read.csv("../../data/Polska/zdarzenia_rsow.csv", sep = "|")
crime <- crime[!is.na(crime$LAT)&!is.na(crime$LNG), ]

# zmiana z , na . w danych (inaczej traktowane są jako string)
crime$LAT <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LAT"]])))
crime$LNG <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LNG"]])))

coordinates(crime) =~ LNG+LAT
projection(crime) = projection(bialystok)

## wybranie przestępstw tylko z Białegostoku
crimeBialystok <- intersect(crime, onlyBialystok)

bialystokOwin <- as.owin(onlyBialystok)
pts <- coordinates(crimeBialystok)
p <- ppp(pts[,1], pts[,2], window = bialystokOwin)
ds_1 <- density(p, adjust = 1, kernel="gaussian", window = kernel)
ds_0.5 <- density(p, adjust = 0.5, kernel="gaussian", window = kernel)
ds_0.25 <- density(p, adjust = 0.25, kernel="gaussian", window = kernel)
ds_0.1 <- density(p, adjust = 0.1, kernel="gaussian", window = kernel)

png(filename = "../plot/KDE/KDE_overall.png", width=2500, height=3000, res=300)
par(mfrow=c(2,2))
plot(ds_1, main = "KDE overall adjust=1, gaussian kernel")
plot(ds_0.5, main = "KDE overall adjust=0.5, gaussian kernel")
plot(ds_0.25, main = "KDE overall adjust=0.25, gaussian kernel")
plot(ds_0.1, main = "KDE overall adjust=0.1, gaussian kernel")
dev.off()

## kategoria
KDEForCategory <- function(categoryName) {
  pts1 <- coordinates(crimeBialystok[crimeBialystok$KAT == categoryName, ])
  p <- ppp(pts1[,1], pts1[,2], window=bialystokOwin)
  
  in_ds_1 <- density(p, adjust = 1, kernel="gaussian", window = kernel)
  in_ds_0.5 <- density(p, adjust = 0.5, kernel="gaussian", window = kernel)
  in_ds_0.25 <- density(p, adjust = 0.25, kernel="gaussian", window = kernel)
  in_ds_0.1 <- density(p, adjust = 0.1, kernel="gaussian", window = kernel)
  
  filename <- paste('../plot/KDE/KDE', categoryName, sep='_')
  filename <- paste(filename, 'png', sep='.')
  
  png(filename = filename, width=2500, height=3000, res=300)
  par(mfrow=c(2,2))
  
  main <- paste('KDE', categoryName, 'adjust=1, gaussian kernel', sep=' ')
  plot(in_ds_1, main = main)
  
  main <- paste('KDE', categoryName, 'adjust=0.5, gaussian kernel', sep=' ')
  plot(in_ds_0.5, main = main)
  
  main <- paste('KDE', categoryName, 'adjust=0.25, gaussian kernel', sep=' ')
  plot(in_ds_0.25, main = main)
  
  main <- paste('KDE', categoryName, 'adjust=0.1, gaussian kernel', sep=' ')
  plot(in_ds_0.1, main = main)
  dev.off()
}

KDEForCategory("CHU")     # 28 537
KDEForCategory("RD")      # 32 182
KDEForCategory("KRA")     # 13 479
KDEForCategory("OŚ")      # 2 509
KDEForCategory("ALK")     # 928
KDEForCategory("LEG")     # 1 185
KDEForCategory("BEZP")    # 8 292
KDEForCategory("ZWIE")    # 2 548
KDEForCategory("PORZ")    # 880
KDEForCategory("GOSP")    # 335





