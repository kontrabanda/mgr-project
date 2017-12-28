library(kknn)
source(file="./model/ClassificationModelClass.R")

kNNModelClass <- setRefClass(
  Class="kNNModelClass",
  fields=list(
    modelFile = "character",
    modelName="character",
    trainData="data.frame"
  ),
  methods = list(
    initialize = function(path = './temp', categories = NULL) {
      modelFile <<- paste(path, 'model.R', sep = '/')
      modelName <<- 'kNNModelClass'
    },
    trainModel = function(data) {
      trainData <<- data
    },
    predictLabels = function(testData) {
      results <- kknn(formula = label~., trainData, testData, k = 10)
      results$prob
    }
  ),
  contains=c("ClassificationModelClass")
)
