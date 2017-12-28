library(e1071)
source(file="./model/ClassificationModelClass.R")

NaiveBayesModelClass <- setRefClass(
  Class="NaiveBayesModelClass",
  fields=list(
    modelFile = "character",
    modelName="character"
  ),
  methods = list(
    initialize = function(path = './temp', categories = NULL) {
      modelFile <<- paste(path, 'model.R', sep = '/')
      modelName <<- 'naiveBayes'
    },
    trainModel = function(data) {
      bayesModel <- naiveBayes(label ~ ., data)
      save(bayesModel, file = modelFile)
    },
    predictLabels = function(test) {
      load(modelFile)
      predict(bayesModel, test, type = 'raw')
    }
  ),
  contains=c("ClassificationModelClass")
)
