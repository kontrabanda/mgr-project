library(liquidSVM)
source(file="ClassificationModelClass.R")

SVMModelClass <- setRefClass(
  Class="SVMModelClass",
  fields=list(
    modelFile = "character",
    modelName="character"
  ),
  methods = list(
    initialize = function(path = './temp', categories = NULL) {
      modelFile <<- paste(path, 'model.R', sep = '/')
      modelName <<- 'svmModelClass'
    },
    trainModel = function(trainData) {
      svmModel <- mcSVM(label~., trainData, threads=-1, partition_choice=6, predict.prob = T)
      save(svmModel, file = modelFile)
    },
    predictLabels = function(testData) {
      load(modelFile)
      pred <- predict(svmModel, testData)
      names(pred) <- gsub("vsOthers", "", names(pred))
      pred
    }
  ),
  contains=c("ClassificationModelClass")
)
