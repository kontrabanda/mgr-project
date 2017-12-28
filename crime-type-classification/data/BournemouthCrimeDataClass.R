library(lubridate)
source(file="./data/DataClass.R")

BournemouthCrimeDataClass <- setRefClass(
  Class="BournemouthCrimeDataClass",
  fields=list(
    rawData="data.frame"
  ),
  methods = list(
    initialize = function() {
      name <<- "bournemouth"
      bostonData <- read.csv(file = "../../data/gb/crimes/crime_only_bournemouth.csv")
      data <- setNames(data.frame(matrix(ncol = 4, nrow = nrow(bostonData))), c("lat", "lng", "date", "category"))
      
      data$date <- bostonData$Month
      data$lat <- bostonData$Latitude
      data$lng <- bostonData$Longitude
      data$category <- bostonData$Crime.type
      rawData <<- data
    },
    getData = function() {
      data <- rawData[, c("lat", "lng", "date")]
      data$label <- rawData$category
      data
    },
    getDataLabeledByCategory = function(categoryName) {
      data <- rawData[, c("lat", "lng", "date")]
      data$label <- ifelse(rawData$category==categoryName, 1, 0)
      data
    },
    getCategoriesNames = function() {
      unique(rawData$category)
    }
  ),
  contains=c("DataClass")
)