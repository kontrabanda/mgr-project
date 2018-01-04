library(lubridate)
source(file="./data/DataClass.R")

BostonCrimeDataClass <- setRefClass(
  Class="BostonCrimeDataClass",
  fields=list(
    rawData="data.frame"
  ),
  methods = list(
    initialize = function() {
      name <<- "boston"
      bostonData <- read.csv(file = "../../data/usa/crimes/crime_only_boston.csv")
      data <- setNames(data.frame(matrix(ncol = 7, nrow = nrow(bostonData))), c("lat", "lng", "hour", "day", "month", "year", "category"))
      
      data$hour <- bostonData$HOUR
      data$day <- bostonData$DAY_OF_WEEK
      data$month <- bostonData$MONTH
      data$year <- bostonData$YEAR
      data$lat <- bostonData$Lat
      data$lng <- bostonData$Long
      data$category <- bostonData$OFFENSE_CODE_GROUP
      data <- removeRareCategories(data)
      data$category <- factor(data$category)
      rawData <<- data
    },
    removeRareCategories = function(data) {
      rareCategories = c(
        'Motor Vehicle Accident Response', 'Larceny', 'Medical Assistance', 'Investigate Person', 'Other', 'Vandalism', 'Drug Violation', 'Simple Assault')
      data[data$category %in% rareCategories,]
    },
    getData = function() {
      data <- rawData[, c("lat", "lng", "month")]
      data$label <- rawData$category
      data
    },
    getDataWithFullDate = function() {
      data <- rawData[, c("lat", "lng", "hour", "day", "month", "year")]
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