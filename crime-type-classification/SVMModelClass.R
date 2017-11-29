library(parallelSVM)
source(file="ClassificationModelClass.R")

SVMModelClass <- setRefClass(
  Class="SVMModelClass",
  fields=list(
    modelFile = "character"
  ),
  methods = list(
    initialize = function(path = './temp', categories = NULL) {
      modelFile <<- paste(path, 'model.R', sep = '/')
    },
    trainModel = function(train) {
      svmModel <- parallelSVM(label~., train, probability = TRUE)
      save(svmModel, file = modelFile)
    },
    predictLabels = function(data) {
      load(modelFile)
      pred <- predict(svmModel, data, probability = TRUE)
      attr(pred, "prob")
    }
  ),
  contains=c("ClassificationModelClass")
)
