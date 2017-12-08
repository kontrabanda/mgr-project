library(caret)
source(file="ClassificationModelClass.R")
source(file="DataClass.R")
source(file="TimeLoggingClass.R")

CheckerWithResultSaving <- setRefClass(
  Class="CheckerWithResultSaving",
  fields=list(
    ClassificationModel="refObjectGenerator",
    crimeDataClass="DataClass",
    resultsPath="character",
    modelPath="character",
    modelName="character",
    folds="integer",
    foldsCount="numeric",
    data="data.frame"
  ),
  methods = list(
    initialize = function(crimeDataClass, ClassificationModel) {
      set.seed(123)
      crimeDataClass <<- crimeDataClass
      ClassificationModel <<- ClassificationModel
      classificationModel <- ClassificationModel(categories = crimeDataClass$getCategoriesNames())
      modelName <<- classificationModel$modelName
      modelPath <<- createModelPath()
      resultsPath <<- createResultsPath()
      data <<- crimeDataClass$getData()
      setFolds(10)
    },
    setFolds = function(k) {
      foldsCount <<- k
      folds <<- createFolds(factor(data$label), k = k, list = FALSE)
    },
    crossValidation = function(sets = 1:foldsCount) {
      for(i in sets) {
        iterationPath <- createIterationModelPath(i)
        classificationModel <- ClassificationModel(iterationPath, crimeDataClass$getCategoriesNames())  
        trainingSingle(classificationModel, i)
        testingSingle(classificationModel, i)
      }
      
      results <- getResults(sets)
      results
    },
    getResults = function(sets = 1:foldsCount) {
      categories <- crimeDataClass$getCategoriesNames()
      results <- data.frame(matrix(NA, nrow = 0, ncol = length(categories)))
      for(i in sets) {
        results <- rbind(results, getSingleResult(i))
      }
      results
    },
    getSingleResult = function(i) {
      result <- read.csv(getIterationResultsPath(i))
      result
    },
    trainingSingle = function(classificationModel, i) {
      timeLoggingClass <- TimeLoggingClass()
      timeLoggingClass$start()
      cat(sprintf('Training %s iteration. Start time: %s \n', i, format(Sys.time(),usetz = TRUE)))
      train <- data[folds != i, ]
      classificationModel$trainModel(train)
      timeLoggingClass$stop()
    },
    testingSingle = function(classificationModel, i) {
      timeLoggingClass <- TimeLoggingClass()
      timeLoggingClass$start()
      cat(sprintf('Testing %s iteration. Start time: %s \n', i, format(Sys.time(),usetz = TRUE)))
      test <- data[folds == i, ]
      testWithoutLabels <- test[, -which(names(test) == "label")]
      results <- classificationModel$predictLabels(testWithoutLabels)
      write.csv(results, file = getIterationResultsPath(i))
      timeLoggingClass$stop()
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
    createIterationModelPath = function(iteration) {
      path <- paste(modelPath, iteration, sep = '/')
      dir.create(path)
      path
    },
    getIterationModelPath = function(iteration) {
      path <- paste(modelPath, iteration, sep = '/')
      path
    },
    createResultsPath = function() {
      dir.create('./results')
      resultsPath <<- paste('./results', crimeDataClass$name, sep = '/')
      dir.create(resultsPath)
      resultsPath <<- paste(resultsPath, modelName, sep = '/')
      dir.create(resultsPath)
      resultsPath
    },
    getIterationResultsPath = function(iteration) {
      path <- paste(resultsPath, iteration, sep = '/')
      path <- paste(path, 'csv', sep = '.')
      path
    }
  )
)