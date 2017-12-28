library(caret)
source(file="./model/ClassificationModelClass.R")
source(file="./data/DataClass.R")
source(file="./time/TimeLoggingClass.R")

ModelCheckerClass <- setRefClass(
  Class="ModelCheckerClass",
  fields=list(
    ClassificationModel="refObjectGenerator",
    crimeDataClass="DataClass",
    modelPath="character",
    modelName="character",
    folds="integer",
    foldsCount="numeric",
    results="data.frame",
    data="data.frame"
  ),
  methods = list(
    initialize = function(crimeDataClass, ClassificationModel) {
      set.seed(123)
      crimeDataClass <<- crimeDataClass
      ClassificationModel <<- ClassificationModel
      classificationModel <- ClassificationModel()
      modelName <<- classificationModel$modelName
      modelPath <<- createModelPath()
      data <<- crimeDataClass$getData()
      setFolds(10)
    },
    setFolds = function(k) {
      foldsCount <<- k
      folds <<- createFolds(factor(data$label), k = k, list = FALSE)
    },
    setResults = function() {
      categories <- crimeDataClass$getCategoriesNames()
      results <<- data.frame(matrix(NA, nrow = 0, ncol = length(categories)))
      colnames(results) <<- categories
    },
    crossValidation = function(trainingSet = 1:foldsCount, testSet = 1:foldsCount) {
      training(trainingSet)
      testing(testSet)
      results
    },
    training = function(trainingSet) {
      for(i in trainingSet) {
        timeLoggingClass$start()
        trainingSingle(i)
        timeLoggingClass$stop()
      }
    },
    trainingSingle = function(i) {
      cat(sprintf('Training %s iteration. Start time: %s \n', i, format(Sys.time(),usetz = TRUE)))
      iterationPath <- createIterationPath(i)
      train <- data[folds != i, ]
      classificationModel <- ClassificationModel(iterationPath, crimeDataClass$getCategoriesNames())  
      classificationModel$trainModel(train)
    },
    testing = function(testSet) {
      for(i in testSet) {
        timeLoggingClass$start()
        testingSingle(i)
        timeLoggingClass$stop()
      }
    },
    testingSingle = function(i) {
      cat(sprintf('Testing %s iteration. Start time: %s \n', i, format(Sys.time(),usetz = TRUE)))
      iterationPath <- getIterationPath(i)
      test <- data[folds == i, ]
      testWithoutLabels <- test[, -which(names(test) == "label")]
      classificationModel <- ClassificationModel(iterationPath, crimeDataClass$getCategoriesNames())  
      results <<- rbind(results, classificationModel$predictLabels(testWithoutLabels))
    },
    getDataWithoutLabels = function(data) {
      data[, -which(names(data) == "label")]
    },
    createModelPath = function() {
      dir.create('./temp')
      modelPath <<- paste('./temp', crimeDataClass$name, sep = '/')
      dir.create(modelPath)
      modelPath <<- paste(modelPath, modelName, sep = '/')
      dir.create(modelPath)
      modelPath
    },
    createIterationPath = function(iteration) {
      path <- paste(modelPath, iteration, sep = '/')
      dir.create(path)
      path
    },
    getIterationPath = function(iteration) {
      path <- paste(modelPath, iteration, sep = '/')
      path
    }
  )
)