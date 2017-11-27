library(caret)
source(file="ClassificationModelClass.R")
source(file="DataClass.R")

ModelCheckerClass <- setRefClass(
  Class="ModelCheckerClass",
  fields=list(
    ClassificationModel="refObjectGenerator",
    crimeDataClass="DataClass",
    modelPath="character",
    modelName="character",
    withTraining="logical"
  ),
  methods = list(
    initialize = function(crimeDataClass, ClassificationModel, modelName, withTraining = TRUE) {
      set.seed(123)
      crimeDataClass <<- crimeDataClass
      ClassificationModel <<- ClassificationModel
      modelName <<- modelName
      withTraining <<- withTraining
      modelPath <<- createModelPath()
    },
    normalChecking = function() {
      data <- crimeDataClass$getData()
      smp_size <- floor(0.8 * nrow(data))
      train_ind <- sample(seq_len(nrow(data)), size = smp_size)
      train <- data[train_ind, ]
      test <- data[-train_ind, ]
      
      classificationModel <- ClassificationModel()
      classificationModel$trainModel(train)
      results <- classificationModel$predictLabels(getDataWithoutLabels(test))
    },
    crossValidation = function(k = 10) {
      data <- crimeDataClass$getData()
      categories <- crimeDataClass$getCategoriesNames()
      folds <- createFolds(factor(data$label), k = k, list = FALSE)
      results <- data.frame(matrix(NA, nrow = 0, ncol = length(categories)))
      
      for(i in 1:k) {
        iterationPath <- createIterationPath(i)
        train <- data[folds != i, ]
        test <- data[folds == i, ]
        colnames(results) <- categories
        
        classificationModel <- ClassificationModel(iterationPath, categories)  
        if(withTraining) {
          classificationModel$trainModel(train)
        }
        testWithoutLabels <- test[, -which(names(test) == "label")]
        results <- rbind(results ,classificationModel$predictLabels(testWithoutLabels))
      }
      
      results
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
    }
  )
)