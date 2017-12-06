library(randomForest)
source(file="ClassificationModelClass.R")

RandomForestModelClass <- setRefClass(
  Class="RandomForestModelClass",
  fields=list(
    modelFile = "character",
    modelName="character"
  ),
  methods = list(
    initialize = function(path = './temp', categories = NULL) {
      modelFile <<- paste(path, 'model.R', sep = '/')
      modelName <<- 'randomForest'
    },
    trainModel = function(trainData) {
      rfModel <- randomForest(label ~ ., data = trainData)
      save(rfModel, file = modelFile)
    },
    predictLabels = function(testData) {
      load(modelFile)
      predict(rfModel, testData, type = 'prob')
    }
  ),
  contains=c("ClassificationModelClass")
)
