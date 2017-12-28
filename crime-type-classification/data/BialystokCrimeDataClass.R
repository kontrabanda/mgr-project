library(lubridate)
source(file="./data/DataClass.R")

BialystokCrimeDataClass <- setRefClass(
  Class="BialystokDataClass",
  fields=list(
    rawData="data.frame"
  ),
  methods = list(
    initialize = function() {
      name <<- "bialystok"
      crimeBialystokDF <- read.csv("../../data/Polska/zdarzenia_rsow_bialystok.csv", sep = ",")
      crimeBialystokDF$DATA <- as.Date(crimeBialystokDF$DATA, "%y/%m/%d")
      
      data <- setNames(data.frame(matrix(ncol = 4, nrow = nrow(crimeBialystokDF))), c("lat", "lng", "date", "category"))
      data$date <- as.factor(month(as.Date(crimeBialystokDF$DATA, "%y/%m/%d")))
      data$lat <- crimeBialystokDF$LAT
      data$lng <- crimeBialystokDF$LNG
      data$category <- crimeBialystokDF$KAT
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