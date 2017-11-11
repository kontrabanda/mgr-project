library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
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

### KDE for Bournemouth
bournemouthOwin <- as.owin(onlyBournemouth)
pts <- coordinates(crimeBournemouth)
p <- ppp(pts[,1], pts[,2], window = bournemouthOwin)
ds_1 <- density(p, adjust = 1, kernel="gaussian", window = kernel)
ds_0.5 <- density(p, adjust = 0.5, kernel="gaussian", window = kernel)
ds_0.25 <- density(p, adjust = 0.25, kernel="gaussian", window = kernel)
ds_0.1 <- density(p, adjust = 0.1, kernel="gaussian", window = kernel)

png(filename = "../plot/KDE/Bournemouth/KDE_overall.png", width=2500, height=3000, res=300)
par(mfrow=c(2,2))
plot(ds_1, main = "KDE Bournemouth overall adjust=1, gaussian kernel")
plot(ds_0.5, main = "KDE Bournemouth overall adjust=0.5, gaussian kernel")
plot(ds_0.25, main = "KDE Bournemouth overall adjust=0.25, gaussian kernel")
plot(ds_0.1, main = "KDE Bournemouth overall adjust=0.1, gaussian kernel")
dev.off()
###

### KDE dla kategorii Bournemouth
KDEForCategory <- function(categoryName) {
  pts1 <- coordinates(crimeBournemouth[crimeBournemouth$Crime.type == categoryName, ])
  p <- ppp(pts1[,1], pts1[,2], window=bournemouthOwin)
  
  in_ds_1 <- density(p, adjust = 1, kernel="gaussian", window = kernel)
  in_ds_0.5 <- density(p, adjust = 0.5, kernel="gaussian", window = kernel)
  in_ds_0.25 <- density(p, adjust = 0.25, kernel="gaussian", window = kernel)
  in_ds_0.1 <- density(p, adjust = 0.1, kernel="gaussian", window = kernel)
  
  filename <- paste('../plot/KDE/Bournemouth/KDE', categoryName, sep='_')
  filename <- paste(filename, 'png', sep='.')
  
  png(filename = filename, width=2500, height=3000, res=300)
  par(mfrow=c(2,2))
  
  main <- paste('KDE Bournemouth', categoryName, 'adjust=1, gaussian kernel', sep=' ')
  plot(in_ds_1, main = main)
  
  main <- paste('KDE Bournemouth', categoryName, 'adjust=0.5, gaussian kernel', sep=' ')
  plot(in_ds_0.5, main = main)
  
  main <- paste('KDE Bournemouth', categoryName, 'adjust=0.25, gaussian kernel', sep=' ')
  plot(in_ds_0.25, main = main)
  
  main <- paste('KDE Bournemouth', categoryName, 'adjust=0.1, gaussian kernel', sep=' ')
  plot(in_ds_0.1, main = main)
  dev.off()
}

categoryNames <- unique(crimeBournemouthDF$Crime.type)
sapply(categoryNames, KDEForCategory)
###
