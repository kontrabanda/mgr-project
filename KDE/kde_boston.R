library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(spatstat)

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

### KDE for Boston
bostonOwin <- as.owin(onlyBoston)
pts <- coordinates(crimeBoston)
p <- ppp(pts[,1], pts[,2], window = bostonOwin)
ds_1 <- density(p, adjust = 1, kernel="gaussian", window = kernel)
ds_0.5 <- density(p, adjust = 0.5, kernel="gaussian", window = kernel)
ds_0.25 <- density(p, adjust = 0.25, kernel="gaussian", window = kernel)
ds_0.1 <- density(p, adjust = 0.1, kernel="gaussian", window = kernel)

png(filename = "../plot/KDE/Boston/KDE_overall.png", width=2500, height=3000, res=300)
par(mfrow=c(2,2))
plot(ds_1, main = "KDE Boston overall adjust=1, gaussian kernel")
plot(ds_0.5, main = "KDE Boston overall adjust=0.5, gaussian kernel")
plot(ds_0.25, main = "KDE Boston overall adjust=0.25, gaussian kernel")
plot(ds_0.1, main = "KDE Boston overall adjust=0.1, gaussian kernel")
dev.off()
###

### KDE dla kategorii Boston
KDEForCategory <- function(categoryName) {
  pts1 <- coordinates(crimeBoston[crimeBoston$OFFENSE_CODE_GROUP == categoryName, ])
  p <- ppp(pts1[,1], pts1[,2], window=bostonOwin)
  
  in_ds_1 <- density(p, adjust = 1, kernel="gaussian", window = kernel)
  in_ds_0.5 <- density(p, adjust = 0.5, kernel="gaussian", window = kernel)
  in_ds_0.25 <- density(p, adjust = 0.25, kernel="gaussian", window = kernel)
  in_ds_0.1 <- density(p, adjust = 0.1, kernel="gaussian", window = kernel)
  
  filename <- paste('../plot/KDE/Boston/KDE', categoryName, sep='_')
  filename <- paste(filename, 'png', sep='.')
  
  png(filename = filename, width=2500, height=3000, res=300)
  par(mfrow=c(2,2))
  
  main <- paste('KDE Boston', categoryName, 'adjust=1, gaussian kernel', sep=' ')
  plot(in_ds_1, main = main)
  
  main <- paste('KDE Boston', categoryName, 'adjust=0.5, gaussian kernel', sep=' ')
  plot(in_ds_0.5, main = main)
  
  main <- paste('KDE Boston', categoryName, 'adjust=0.25, gaussian kernel', sep=' ')
  plot(in_ds_0.25, main = main)
  
  main <- paste('KDE Boston', categoryName, 'adjust=0.1, gaussian kernel', sep=' ')
  plot(in_ds_0.1, main = main)
  dev.off()
}

# jak wymienimy / na pusty string to nie złapie name tego w powyższej funkcji
#categoryNames <- unique(crimeBostonDF$OFFENSE_CODE_GROUP)
#categoryNames <- gsub("/", "", categoryNames)
#sapply(categoryNames, KDEForCategory)

KDEForCategory("Motor Vehicle Accident Response")
KDEForCategory("Larceny")
KDEForCategory("Medical Assistance")
KDEForCategory("Investigate Person")
KDEForCategory("Other")
KDEForCategory("Vandalism")
KDEForCategory("Drug Violation")
KDEForCategory("Simple Assault")
KDEForCategory("Verbal Disputes")
KDEForCategory("Larceny From Motor Vehicle")
KDEForCategory("Towed")
KDEForCategory("Investigate Property")
KDEForCategory("Property Lost")
KDEForCategory("Warrant Arrests")
KDEForCategory("Aggravated Assault")
###
