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
      bournemouthData <- read.csv(file = "../../data/gb/crimes/crime_only_bournemouth.csv")
      data <- setNames(data.frame(matrix(ncol = 4, nrow = nrow(bournemouthData))), c("lat", "lng", "month", "category"))
      
      data$month <- bournemouthData$Month
      data$lat <- bournemouthData$Latitude
      data$lng <- bournemouthData$Longitude
      data$category <- bournemouthData$Crime.type
      rawData <<- data
    },
    getData = function() {
      data <- rawData[, c("lat", "lng", "month")]
      data$label <- rawData$category
      data
    },
    getDataWithFullDate = function() {
      data <- rawData[, c("lat", "lng", "month")]
      data$label <- rawData$category
      data
    },
    getDataLabeledByCategory = function(categoryName) {
      data <- rawData[, c("lat", "lng", "month")]
      data$label <- ifelse(rawData$category==categoryName, 1, 0)
      data
    },
    getCategoriesNames = function() {
      unique(rawData$category)
    }
  ),
  contains=c("DataClass")
)